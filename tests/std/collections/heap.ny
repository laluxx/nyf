;;; heap.ny --- collections heap module
;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: collections heap
;;; Commentary:
;; collections heap module
fn heap() {
    "Min-heap using list."
    return list(8)
}

fn hpush(h, v) {
    "Function: hpush."
    h = append(h, v)
    def i = list_len(h) - 1
    while i>0 {
        def p = i-1 / 2
        if get(h, p) <= get(h, i) {
            i = 0
        } else {
            def tmp = get(h, p)
            set_idx(h, p, get(h, i))
            set_idx(h, i, tmp)
            i = p
        }
    }
    return h
}

fn heap_push(self, v) {
    "Function: heap_push."
    return hpush(self, v)
}

fn hpop(h) {
    "Function: hpop."
    def n = list_len(h)
    if n == 0 {
        return 0
    }
    def out = get(h, 0)
    def last = pop(h)
    n = n - 1
    if n>0 {
        set_idx(h, 0, last)
        def i = 0
        def done = 0
        while done == 0 {
            def l = i*2+1
            r = i*2+2
            m = i
            if l < n && get(h, l) < get(h, m) {
                m = l
            }
            if r < n && get(h, r) < get(h, m) {
                m = r
            }
            if m == i {
                done = 1
            } else {
                def tmp = get(h, i)
                set_idx(h, i, get(h, m))
                set_idx(h, m, tmp)
                i = m
            }
        }
    }
    return out
}

fn heap_pop(self) {
    "Function: heap_pop."
    return hpop(self)
}

fn hpeek(h) {
    "Function: hpeek."
    if list_len(h) == 0 {
        return 0
    }
    return get(h, 0)
}
