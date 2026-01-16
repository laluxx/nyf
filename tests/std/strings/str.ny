use std.core.mod
fn str_clone(s) {
    "Create a deep copy of string `s`."
    def n = str_len(s)
    def p = rt_malloc(n + 1)
    store64(p, 120, -8)
    rt_memcpy(p, s, n)
    store8(p, 0, n)
    return p
}

fn cstr_to_str(s, offset = 0) {
    "Convert a C string at `s + offset` to a Nytrix string."
    def n = 0
    while rt_load8_idx(s, offset + n) != 0 {
        n = n + 1
    }
    def p = rt_malloc(n + 1)
    store64(p, 120, -8)
    rt_memcpy(p, ptr_add(s, offset), n)
    store8(p, 0, n)
    return p
}

fn str_len(s) {
    "Return the number of bytes in string `s` (excluding null terminator)."
    return rt_str_len(s)
}

fn concat(a, b) {
    "Return a new string that is the result of joining strings `a` and `b`."
    return rt_str_concat(a, b)
}

fn slice(s, start, stop, step = 1) {
    "Return a substring of `s` from indices `start` to `stop` with `step`."
    def len = str_len(s)
    if start < 0 {
        start = len + start
    }
    if stop < 0 {
        stop = len + stop
    }
    if step > 0 {
        if start < 0 {
            start = 0
        }
        if stop > len {
            stop = len
        }
        if start >= stop {
            return ""
        }
    } else {
        if start >= len {
            start = len - 1
        }
        if stop < -1 {
            stop = -1
        }
        if start <= stop {
            return ""
        }
    }
    def out_len = 0
    if step > 0 {
        out_len = stop - start + step - 1 / step
    } else {
        out_len = start - stop - step - 1 / 0 - step
    }
    if out_len <= 0 {
        return ""
    }
    def out = rt_malloc(out_len + 1)
    store64(out, 120, -8)
    def i = start
    def oi = 0
    if step > 0 {
        while i < stop {
            rt_store8_idx(out, oi, rt_load8_idx(s, i))
            oi = oi + 1
            i = i + step
        }
    } else {
        while i > stop {
            rt_store8_idx(out, oi, rt_load8_idx(s, i))
            oi = oi + 1
            i = i + step
        }
    }
    rt_store8_idx(out, oi, 0)
    return out
}

fn find(s, sub) {
    "Return the first index of substring `sub` in `s`, or -1 if not found."
    def ls = str_len(s)
    def lp = str_len(sub)
    if ls < lp {
        return -1
    }
    if lp == 0 {
        return 0
    }
    def i = 0
    while i <= ls - lp {
        def j = 0
        def is_match = 1
        while j < lp {
            def char_s = rt_load8_idx(s, i + j)
            def char_sub = rt_load8_idx(sub, j)
            if char_s != char_sub {
                is_match = 0
                break
            }
            j = j + 1
        }
        if is_match == 1 {
            return i
        }
        i = i + 1
    }
    return -1
}

fn split(s, sep) {
    "Splits string `s` into a list of strings using `sep` as the delimiter."
    def res = list(8)
    def start = 0
    def n = str_len(s)
    def sn = str_len(sep)
    if sn == 0 {
        return res
    }
    def i = 0
    while i <= n - sn {
        def is_match = 1
        def j = 0
        while j < sn {
            if rt_load8_idx(s, i + j) != rt_load8_idx(sep, j) {
                is_match = 0
                break
            }
            j = j + 1
        }
        if is_match {
            res = append(res, slice(s, start, i, 1))
            start = i + sn
            i = start
        } else {
            i = i + 1
        }
    }
    res = append(res, slice(s, start, n, 1))
    return res
}

fn join(xs, sep) {
    "Joins a list of strings `xs` into a single string, with `sep` placed between each element."
    def n = list_len(xs)
    if n == 0 {
        return ""
    }
    if n == 1 {
        def s = get(xs, 0)
        if is_int(s) {
            return itoa(s)
        }
        return s
    }
    def res = get(xs, 0)
    if is_int(res) {
        res = itoa(res)
    }
    def i = 1
    while i < n {
        res = concat(res, sep)
        def s = get(xs, i)
        if is_int(s) {
            s = itoa(s)
        }
        res = concat(res, s)
        i = i + 1
    }
    return res
}

fn partition(s, sep) {
    "Splits string `s` at the first occurrence of `sep`, returning a list [before, sep, after]."
    def idx = find(s, sep)
    if idx < 0 {
        return [s, "", ""]
    }
    def sn = str_len(sep)
    return [slice(s, 0, idx, 1), sep, slice(s, idx + sn, str_len(s), 1)]
}

fn replace_all(s, old, nw) {
    "Return a new string where all occurrences of `old` in `s` are replaced with `nw`."
    def parts = split(s, old)
    return join(parts, nw)
}

fn count(s, sub) {
    def n = str_len(s)
    def m = str_len(sub)
    if m == 0 {
        return 0
    }
    def res = 0
    def i = 0
    while i <= n - m {
        def is_match = 1
        def j = 0
        while j < m {
            if rt_load8_idx(s, i + j) != rt_load8_idx(sub, j) {
                is_match = 0
                break
            }
            j = j + 1
        }
        if is_match == 1 {
            res = res + 1
            i = i + m
        } else {
            i = i + 1
        }
    }
    return res
}

fn strip(s) {
    "Return a copy of string `s` with leading and trailing whitespace removed."
    if s == 0 {
        return ""
    }
    def n = str_len(s)
    def start = 0
    while start < n {
        def c = rt_load8_idx(s, start)
        if c != 32 && c != 10 && c != 13 && c != 9 {
            break
        }
        start = start + 1
    }
    if start == n {
        return ""
    }
    def end = n - 1
    while end > start {
        def c = rt_load8_idx(s, end)
        if c != 32 && c != 10 && c != 13 && c != 9 {
            break
        }
        end = end - 1
    }
    return slice(s, start, end + 1, 1)
}

fn lstrip(s) {
    "Return a copy of string `s` with leading whitespace removed."
    if s == 0 {
        return ""
    }
    def n = str_len(s)
    def start = 0
    while start < n {
        def c = rt_load8_idx(s, start)
        if c != 32 && c != 10 && c != 13 && c != 9 {
            break
        }
        start = start + 1
    }
    return slice(s, start, n, 1)
}

fn rstrip(s) {
    "Return a copy of string `s` with trailing whitespace removed."
    if s == 0 {
        return ""
    }
    def n = str_len(s)
    def end = n - 1
    while end >= 0 {
        def c = rt_load8_idx(s, end)
        if c != 32 && c != 10 && c != 13 && c != 9 {
            break
        }
        end = end - 1
    }
    return slice(s, 0, end + 1, 1)
}

fn upper(s) {
    "Return a copy of string `s` with all lowercase characters converted to uppercase."
    def n = str_len(s)
    def out = rt_malloc(n + 1)
    store64(out, 120, -8)
    def i = 0
    while i < n {
        def c = rt_load8_idx(s, i)
        if c >= 97 && c <= 122 {
            rt_store8_idx(out, i, c - 32)
        } else {
            rt_store8_idx(out, i, c)
        }
        i = i + 1
    }
    rt_store8_idx(out, n, 0)
    return out
}

fn lower(s) {
    "Return a copy of string `s` with all uppercase characters converted to lowercase."
    def n = str_len(s)
    def out = rt_malloc(n + 1)
    store64(out, 120, -8)
    def i = 0
    while i < n {
        def c = rt_load8_idx(s, i)
        if c >= 65 && c <= 90 {
            rt_store8_idx(out, i, c + 32)
        } else {
            rt_store8_idx(out, i, c)
        }
        i = i + 1
    }
    rt_store8_idx(out, n, 0)
    return out
}

fn repeat(s, n) {
    "Return a new string consisting of `s` repeated `n` times."
    if n <= 0 {
        return ""
    }
    def res = ""
    def i = 0
    while i < n {
        res = concat(res, s)
        i = i + 1
    }
    return res
}

fn splitlines(s) {
    "Splits string `s` at newline characters, returning a list of lines."
    return split(s, "\\n")
}

fn pad_start(s, width, fill = " ") {
    "Pads string `s` on the left with `fill` character until it reaches `width`."
    if fill == 0 {
        fill = " "
    }
    def l = str_len(s)
    if l >= width {
        return s
    }
    def diff = width - l
    def out = ""
    def i = 0
    while i < diff {
        out = concat(out, fill)
        i = i + 1
    }
    return concat(out, s)
}

fn pad_end(s, width, fill = " ") {
    "Pads string `s` on the right with `fill` character until it reaches `width`."
    if fill == 0 {
        fill = " "
    }
    def l = str_len(s)
    if l >= width {
        return s
    }
    def diff = width - l
    def out = s
    def i = 0
    while i < diff {
        out = concat(out, fill)
        i = i + 1
    }
    return out
}

fn zfill(s, width) {
    "Pads string `s` with zeros on the left until it reaches `width`. Handles leading sign character."
    def l = str_len(s)
    if l >= width {
        return s
    }
    if rt_load8_idx(s, 0) == 45 {
        return concat("-", pad_start(slice(s, 1, l, 1), width - 1, "0"))
    }
    return pad_start(s, width, "0")
}

fn chr(code) {
    "Return a single-character string containing the character with the given ASCII/Unicode code point."
    def p = rt_malloc(2)
    store64(p, 120, -8)
    rt_store8_idx(p, 0, code)
    rt_store8_idx(p, 1, 0)
    return p
}

fn ord(s) {
    "Return the numeric code point of the first character in string `s`."
    return rt_load8_idx(s, 0)
}

fn itoa(n) {
    "Convert an integer to its decimal string representation."
    return rt_to_str(n)
}

fn atoi(s) {
    "Parses an integer from string `s`."
    if s == 0 {
        return 0
    }
    def n = str_len(s)
    if n == 0 {
        return 0
    }
    def i = 0
    while i < n {
        def c = rt_load8_idx(s, i)
        if c != 32 && c != 9 && c != 10 && c != 13 {
            break
        }
        i = i + 1
    }
    if i == n {
        return 0
    }
    def sign = 1
    def c = rt_load8_idx(s, i)
    if c == 45 {
        sign = -1
        i = i + 1
    } else {
        if c == 43 {
            i = i + 1
        }
    }
    def res = 0
    while i < n {
        def c = rt_load8_idx(s, i)
        if c < 48 || c > 57 {
            break
        }
        res = res * 10 + c - 48
        i = i + 1
    }
    return res * sign
}

fn startswith(s, prefix) {
    "Return true if string `s` starts with `prefix`."
    def n = str_len(prefix)
    if str_len(s) < n {
        return false
    }
    def i = 0
    while i < n {
        if rt_load8_idx(s, i) != rt_load8_idx(prefix, i) {
            return false
        }
        i = i + 1
    }
    return true
}

fn endswith(s, suffix) {
    "Return true if string `s` ends with `suffix`."
    def n = str_len(suffix)
    def len = str_len(s)
    if len < n {
        return false
    }
    def start = len - n
    def i = 0
    while i < n {
        if rt_load8_idx(s, start + i) != rt_load8_idx(suffix, i) {
            return false
        }
        i = i + 1
    }
    return true
}
