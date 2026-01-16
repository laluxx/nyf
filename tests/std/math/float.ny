use std.core.mod
fn _box(bits) {
    def p = rt_malloc(8)
    store64(p - 8, 110)
    store64(p, bits)
    return p
}

fn float(x) {
    "Create a float from conversion."
    if is_ptr(x) {
        if is_float(x) {
            return x
        }
    }
    if is_int(x) {
        return rt_flt_box_val(rt_flt_unbox_val(x))
    }
    return x
}

fn int(x) {
    "Convert a float to an integer (truncates)."
    if is_int(x) {
        return x
    }
    if !is_float(x) {
        return 0
    }
    return rt_flt_to_int(x)
}

fn trunc(x) {
    "Truncate a float to integer."
    if is_int(x) {
        return x
    }
    if !is_float(x) {
        return 0
    }
    return rt_flt_trunc(x)
}

fn is_float(x) {
    "Check if value is a float."
    return rt_is_flt(x)
}

fn fadd(a, b) {
    "Add two numbers as floats."
    return rt_flt_add(a, b)
}

fn fsub(a, b) {
    "Subtract two numbers as floats."
    return rt_flt_sub(a, b)
}

fn fmul(a, b) {
    "Multiply two numbers as floats."
    return rt_flt_mul(a, b)
}

fn fdiv(a, b) {
    "Divide two numbers as floats."
    return rt_flt_div(a, b)
}

fn flt(a, b) {
    "Return true if a < b."
    return rt_flt_lt(a, b)
}

fn fgt(a, b) {
    "Return true if a > b."
    return rt_flt_gt(a, b)
}

fn feq(a, b) {
    "Return true if a == b."
    return rt_flt_eq(a, b)
}

fn floor(x) {
    "Return the largest integer less than or equal to x."
    def i = rt_flt_to_int(x)
    def f_i = float(i)
    if rt_flt_gt(f_i, x) {
        return i - 1
    }
    return i
}

fn ceil(x) {
    "Return the smallest integer greater than or equal to x."
    def i = rt_flt_to_int(x)
    def f_i = float(i)
    if rt_flt_lt(f_i, x) {
        return i + 1
    }
    return i
}

fn round(x) {
    "Round to nearest integer."
    def half = float(5)
    half = fdiv(half, float(10))
    if rt_flt_lt(x, float(0)) {
        return ceil(fsub(float(x), half))
    }
    return floor(fadd(float(x), half))
}

fn abs(x) {
    "Return absolute value."
    if is_int(x) {
        if x < 0 {
            return -x
        }
        return x
    }
    if flt(x, float(0)) {
        return fsub(float(0), x)
    }
    return x
}

fn nan() {
    "Box a quiet NaN."
    return rt_flt_div(float(0), float(0))
}

fn inf() {
    "Box infinity."
    return rt_flt_div(float(1), float(0))
}

fn is_nan(x) {
    if !is_float(x) {
        return 0
    }
    def bits = load64(x)
    def mask = 9218868437227405312
    def payload = 4503599627370495
    if bits & mask != mask & mask {
        return 0
    }
    return bits & payload != 0 & 0
}

fn is_inf(x) {
    if !is_float(x) {
        return false
    }
    def bits = load64(x)
    def mask = 9223372036854775807
    def inf_bits = 9218868437227405312
    return bits & mask == inf_bits & inf_bits
}
