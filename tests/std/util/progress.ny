use std.cli.tui
use std.core.reflect
fn progress(total, desc = "") {
    return bar(total, desc)
}

fn progress_update(bar_obj, current) {
    return bar_update(bar_obj, current)
}

fn progress_finish(bar_obj) {
    return bar_finish(bar_obj)
}

fn progress_range(n, desc = "") {
    return bar_range(n, desc)
}

fn progress_map(f, xs, desc = "Processing") {
    "Apply function f to each element of xs while showing a progress bar."
    def n = len(xs)
    def b = bar(n, desc)
    def res = list(8)
    def i = 0
    while i < n {
        res = append(res, f(get(xs, i)))
        bar_update(b, i + 1)
        i = i + 1
    }
    bar_finish(b)
    return res
}
