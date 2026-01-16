;;; queue.ny --- collections queue module
;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: collections queue
;;; Commentary:
;; collections queue module
fn queue() {
    "Simple FIFO queue using list."
    return list(8)
}

fn enqueue(q, v) {
    "Function: enqueue."
    return append(q, v)
}

fn queue_enqueue(self, v) {
    "Function: queue_enqueue."
    return enqueue(self, v)
}

fn dequeue(q) {
    "Function: dequeue."
    if list_len(q) == 0 {
        return 0
    }
    def v = get(q, 0)
    def i = 1
    n = list_len(q)
    while i < n {
        set_idx(q, i-1, get(q, i))
        i = i+1
    }
    pop(q)
    return v
}

fn queue_dequeue(self) {
    "Function: queue_dequeue."
    return dequeue(self)
}

fn qlen(q) {
    "Function: qlen."
    return list_len(q)
}
