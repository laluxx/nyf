;;; convert.ny --- util convert module
;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: util convert
;;; Commentary:
;; Type conversion utilities - OOP-style API
fn str(val) {
    "Convert any value to string representation."
    def t = type(val)
    if _str_eq(t, "int") {
        return int_to_str(val)
    }
    if _str_eq(t, "bool") {
        if val {
            return "true"
        }
        return "false"
    }
    if _str_eq(t, "str") {
        return val
    }
    return repr(val)
}

fn int_to_str(n) {
    "Convert integer/float to string."
    if n == 0 {
        return "0"
    }
    def negative = 0
    if n < 0 {
        negative = 1
        n = -n
    }
    def temp = n
    def digits = 0
    while temp > 0 {
        digits = digits + 1
        temp = temp / 10
    }
    def size = digits
    size = digits
    if negative {
        size = size + 1
    }
    def buf = rt_malloc(size + 1)
    rt_store64_idx(buf, -8, 120)
    def pos = size
    rt_store8_idx(buf, pos, 0)
    pos = pos - 1
    while n > 0 {
        def digit = n % 10
        rt_store8_idx(buf, pos, 48 + digit)
        pos = pos - 1
        n = n / 10
    }
    if negative {
        rt_store8_idx(buf, 0, 45)
    }
    return buf
}

fn parse_int(s) {
    "Convert string to int."
    if _str_eq(type(s), "int") {
        return s
    }
    def n = len(s)
    if n == 0 {
        return 0
    }
    def result = 0
    def negative = 0
    def start = 0
    if rt_load8_idx(s, 0) == 45 {
        negative = 1
        start = 1
    }
    def i = start
    while i < n {
        def c = rt_load8_idx(s, i)
        if c >= 48 && c <= 57 {
            result = result * 10 + c - 48
        }
        i = i + 1
    }
    if negative {
        return -result
    }
    return result
}

fn to_bool(val) {
    "Convert to bool."
    def t = type(val)
    if _str_eq(t, "bool") {
        return val
    }
    if _str_eq(t, "int") {
        return val != 0
    }
    if _str_eq(t, "str") {
        return len(val) > 0
    }
    if _str_eq(t, "list") {
        return list_len(val) > 0
    }
    return val != 0
}

fn convert_str(self) {
    "Method aliases for OOP-style API."
    return str(self)
}

fn convert_int(self) {
    "Function: convert_int."
    return parse_int(self)
}

fn convert_bool(self) {
    "Function: convert_bool."
    return to_bool(self)
}
"Type/data conversion helpers (int<->str, etc.)."
