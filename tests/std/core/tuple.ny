use std.core.mod
fn tuple(xs) {
    "Create a new tuple from a list of elements. Tuples are immutable versions of lists."
    if is_list(xs) == false {
        return tuple([])
    }
    def out = list_clone(xs)
    store64(out - 8, 103)
    return out
}

fn is_tuple(x) {
    "Check if a value is a tuple."
    if !is_ptr(x) {
        return false
    }
    return load64(x - 8) == 103
}
