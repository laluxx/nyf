fn apply(f, x) {
    return f(x)
}

fn double(x) {
    return x * 2
}

define result = apply(double, 21)
assert(42, result, "Result is not 42")
