use std.core.mod
use std.collections.mod
fn mean(xs) {
    "Mean of list of numbers."
    def n = list_len(xs)
    if n == 0 {
        return 0
    }
    def s = 0
    def i = 0
    while i < n {
        s = s + get(xs, i)
        i = i + 1
    }
    return s / n
}

fn stat_mean(self) {
    "Method-style alias."
    return mean(self)
}

fn median(xs) {
    "Median (simple sort copy)."
    def n = list_len(xs)
    if n == 0 {
        return 0
    }
    def tmp = list_clone(xs)
    sort(tmp)
    def mid = n/2
    if n % 2 == 1 {
        return get(tmp, mid)
    }
    return get(tmp, mid-1) + get(tmp, mid) / 2
}

fn stat_median(self) {
    "Method-style alias."
    return median(self)
}
