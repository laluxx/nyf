use std.core.mod
use std.core.reflect
use std.collections.dict
use std.collections.set
use std.collections.sort
fn contains(obj, x) {
    if is_list(obj) {
        return list_contains(obj, x)
    }
    if is_dict(obj) {
        return has(obj, x)
    }
    if is_set(obj) {
        return set_contains(obj, x)
    }
    if is_str(obj) {
        return find(obj, x) >= 0
    }
    return false
}

fn list_contains(lst, x) {
    "Return true if list `lst` contains item `x`, false otherwise."
    def i = 0
    def n = list_len(lst)
    while i < n {
        if eq(get(lst, i), x) {
            return true
        }
        i = i + 1
    }
    return false
}

fn list_reversed(lst) {
    "Return a new list with elements in reverse order."
    def n = list_len(lst)
    def out = list(8)
    def i = n - 1
    while i >= 0 {
        out = append(out, get(lst, i))
        i = i - 1
    }
    return out
}
