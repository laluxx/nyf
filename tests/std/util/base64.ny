use std.core.mod
use std.strings.str
fn _b64_table() {
    return "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
}

fn b64_index(c) {
    if c >= 65 && c <= 90 {
        return c - 65
    }
    if c >= 97 && c <= 122 {
        return c - 71
    }
    if c >= 48 && c <= 57 {
        return c + 4
    }
    if c == 43 {
        return 62
    }
    if c == 47 {
        return 63
    }
    return -1
}

fn b64_encode(s) {
    "Function: b64_encode."
    def n = str_len(s)
    def out_len = n + 2 / 3 * 4
    def out = rt_malloc(out_len + 1)
    rt_store64_idx(out, -8, 120)
    def i = 0
    o = 0
    def t = _b64_table()
    while i < n {
        def b0 = rt_load8_idx(s, i)
        def b1 = 0
        b2 = 0
        def idx1 = i + 1
        if idx1 < n {
            b1 = rt_load8_idx(s, idx1)
        }
        def idx2 = i + 2
        if idx2 < n {
            b2 = rt_load8_idx(s, idx2)
        }
        def triple = b0 << 16 | b1 << 8 | b2
        rt_store8_idx(out, o, rt_load8_idx(t, triple >> 18 & 63))
        o = o+1
        rt_store8_idx(out, o, rt_load8_idx(t, triple >> 12 & 63))
        o = o+1
        if idx1 < n {
            rt_store8_idx(out, o, rt_load8_idx(t, triple >> 6 & 63))
        } else {
            rt_store8_idx(out, o, 61)
        }
        o = o+1
        if idx2 < n {
            rt_store8_idx(out, o, rt_load8_idx(t, triple & 63))
        } else {
            rt_store8_idx(out, o, 61)
        }
        o = o+1
        i = i+3
    }
    rt_store8_idx(out, o, 0)
    return out
}

fn b64_decode(s) {
    "Function: b64_decode."
    def n = str_len(s)
    def out = rt_malloc(n + 1)
    rt_store64_idx(out, -8, 120)
    def i = 0
    o = 0
    while i < n {
        def c0 = rt_load8_idx(s, i)
        def c1 = rt_load8_idx(s, i+1)
        def c2 = rt_load8_idx(s, i+2)
        def c3 = rt_load8_idx(s, i+3)
        if c0 == 0 || c1 == 0 {
            break
        }
        def v0 = b64_index(c0)
        def v1 = b64_index(c1)
        if v0 < 0 || v1 < 0 {
            break
        }
        def triple = v0 << 18 | v1 << 12
        def v2 = 0
        if c2 != 61 {
            v2 = b64_index(c2)
            triple = triple | v2 << 6
        }
        def v3 = 0
        if c3 != 61 {
            v3 = b64_index(c3)
            triple = triple | v3
        }
        def b0 = triple >> 16 & 255
        rt_store8_idx(out, o, b0)
        o = o + 1
        if c2 != 61 {
            def b1 = triple >> 8 & 255
            rt_store8_idx(out, o, b1)
            o = o + 1
        }
        if c3 != 61 {
            def b2 = triple & 255
            rt_store8_idx(out, o, b2)
            o = o + 1
        }
        i = i + 4
    }
    rt_store8_idx(out, o, 0)
    return out
}
