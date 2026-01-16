use std.core.mod
use std.core.error
use std.collections.dict
use std.strings.str
use std.collections.mod
fn _be_bytes(n, count) {
    def b = list(8)
    def i = count - 1 * 8
    while i >= 0 {
        b = append(b, n >> i & 255)
        i = i - 8
    }
    return b
}

fn _append(lst, b) {
    return append(lst, b & 255)
}

fn _append_many(lst, src) {
    def i = 0
    n = list_len(src)
    while i < n {
        lst = append(lst, get(src, i))
        i = i+1
    }
    return lst
}

fn msgpack_encode(v) {
    "Encodes a Nytrix value to MessagePack byte list (supports int,str,list,dict)."
    def out = list(8)
    return _mp_enc(v, out)
}

fn _mp_enc(v, out) {
    if is_dict(v) {
        def n = list_len(v)
        if n < 16 {
            out = _append(out, 128 | n)
        } else {
            out = _append(out, 222)
            out = _append(out, n >> 8)
            out = _append(out, n & 255)
        }
        def pairs = items(v)
        def i = 0
        while i < n {
            def p = get(pairs, i)
            out = _mp_enc(get(p, 0), out)
            out = _mp_enc(get(p, 1), out)
            i = i+1
        }
        return out
    }
    if is_list(v) || is_tuple(v) {
        n = list_len(v)
        if n < 16 {
            out = _append(out, 144 | n)
        } else {
            out = _append(out, 220)
            out = _append(out, n >> 8)
            out = _append(out, n & 255)
        }
        def i = 0
        while i < n {
            out = _mp_enc(get(v, i), out)
            i = i+1
        }
        return out
    }
    if is_str(v) {
        v = v + ""
        def l = str_len(v)
        if l < 32 {
            out = _append(out, 160 | l)
        } else {
            out = _append(out, 217)
            out = _append(out, l)
        }
        def i = 0
        def b = 0
        while i < l {
            b = rt_load8_idx(v, i)
            out = _append(out, b)
            i = i+1
        }
        return out
    }
    if v >= 0 {
        if v < 128 {
            out = _append(out, v)
            return out
        }
        if v < 256 {
            out = _append(out, 204)
            out = _append(out, v)
            return out
        }
        if v < 65536 {
            out = _append(out, 205)
            out = _append_many(out, _be_bytes(v, 2))
            return out
        }
        if v < 4294967296 {
            out = _append(out, 206)
            out = _append_many(out, _be_bytes(v, 4))
            return out
        }
        out = _append(out, 207)
        out = _append_many(out, _be_bytes(v, 8))
        return out
    }
    if v >= -32 {
        out = _append(out, v)
        return out
    }
    if v >= -128 {
        out = _append(out, 208)
        out = _append(out, v)
        return out
    }
    if v >= -32768 {
        out = _append(out, 209)
        out = _append_many(out, _be_bytes(v, 2))
        return out
    }
    if v >= -2147483648 {
        out = _append(out, 210)
        out = _append_many(out, _be_bytes(v, 4))
        return out
    }
    out = _append(out, 211)
    out = _append_many(out, _be_bytes(v, 8))
    return out
}

fn msgpack_decode(bytes) {
    "Decodes a MessagePack byte list produced by msgpack_encode."
    def p = _mp_dec(bytes, 0)
    return get(p, 0)
}

fn msgpack_decode_from(bytes, idx) {
    "Decodes one value from `bytes` starting at `idx`. Returns `[value, next_idx]`."
    return _mp_dec(bytes, idx)
}

fn msgpack_stream_decode(bytes) {
    "Decodes a sequence of concatenated MessagePack objects. Returns list of values."
    def out = list(8)
    def i = 0
    n = list_len(bytes)
    while i < n {
        def p = _mp_dec(bytes, i)
        out = append(out, get(p, 0))
        i = get(p, 1)
    }
    return out
}

fn _mp_dec(bytes, i) {
    def b = get(bytes, i)
    i = i+1
    if b < 128 || b >= 224 {
        return [b, i]
    }
    if b & 240 == 128 {
        def n = b & 15
        def m = dict(16)
        def j = 0
        while j < n {
            pk = _mp_dec(bytes, i)
            k = get(pk, 0)
            i = get(pk, 1)
            pv = _mp_dec(bytes, i)
            v = get(pv, 0)
            i = get(pv, 1)
            m = setitem(m, k, v)
            j = j+1
        }
        return [m, i]
    }
    if b & 240 == 144 {
        n = b & 15
        def lst = list(8)
        j = 0
        while j < n {
            pv = _mp_dec(bytes, i)
            lst = append(lst, get(pv, 0))
            i = get(pv, 1)
            j = j+1
        }
        return [lst, i]
    }
    if b & 224 == 160 {
        def l = b & 31
        def s = rt_malloc(l+1)
        rt_store64_idx(s, -8, 120)
        j = 0
        while j < l {
            rt_store8_idx(s, j, get(bytes, i+j))
            j = j+1
        }
        rt_store8_idx(s, l, 0)
        return [s, i+l]
    }
    if b == 217 {
        l = get(bytes, i)
        i = i+1
        s = rt_malloc(l+1)
        rt_store64_idx(s, -8, 120)
        j = 0
        while j < l {
            rt_store8_idx(s, j, get(bytes, i+j))
            j = j+1
        }
        rt_store8_idx(s, l, 0)
        return [s, i+l]
    }
    if b == 220 {
        l = get(bytes, i) << 8 | get(bytes, i+1)
        i = i+2
        lst = list(8)
        j = 0
        while j < l {
            pv = _mp_dec(bytes, i)
            lst = append(lst, get(pv, 0))
            i = get(pv, 1)
            j = j+1
        }
        return [lst, i]
    }
    if b == 222 {
        l = get(bytes, i) << 8 | get(bytes, i+1)
        i = i+2
        m = dict(16)
        j = 0
        while j < l {
            pk = _mp_dec(bytes, i)
            k = get(pk, 0)
            i = get(pk, 1)
            pv = _mp_dec(bytes, i)
            v = get(pv, 0)
            i = get(pv, 1)
            m = setitem(m, k, v)
            j = j+1
        }
        return [m, i]
    }
    if b == 208 {
        def v = get(bytes, i)
        if v > 127 {
            v = v - 256
        }
        return [v, i+1]
    }
    if b == 209 {
        def v = get(bytes, i) << 8 | get(bytes, i+1)
        if v > 32767 {
            v = v - 65536
        }
        return [v, i+2]
    }
    if b == 210 {
        def v = get(bytes, i) << 24 | get(bytes, i+1) << 16 | get(bytes, i+2) << 8 | get(bytes, i+3)
        if v > 2147483647 {
            v = v - 4294967296
        }
        return [v, i+4]
    }
    if b == 204 {
        return [get(bytes, i), i+1]
    }
    if b == 205 {
        def v = get(bytes, i) << 8 | get(bytes, i+1)
        return [v, i+2]
    }
    if b == 206 {
        def v = get(bytes, i) << 24 | get(bytes, i+1) << 16 | get(bytes, i+2) << 8 | get(bytes, i+3)
        return [v, i+4]
    }
    if b == 207 || b == 211 {
        def val = 0
        j = 0
        while j < 8 {
            val = val << 8 | get(bytes, i+j)
            j = j+1
        }
        i = i+8
        return [val, i]
    }
    panic("msgpack: unsupported type")
}
