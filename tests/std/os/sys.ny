;;; sys.ny --- os sys module
;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: os sys
;;; Commentary:
;; Raw system interface
fn syscall(num, a = 0, b = 0, c = 0, d = 0, e = 0, f = 0) {
    "Raw syscall (Linux x86_64): syscall(num, a=0,b=0,c=0,d=0,e=0,f=0)."
    return rt_syscall(num, a, b, c, d, e, f)
}

fn errno() {
    "Get last error code."
    return rt_errno()
}
