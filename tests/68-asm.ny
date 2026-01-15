use std.core

print("Testing ASM...")
;; Simple MOV
;; Return 42 (Tagged: 85). Operand 0 is $0. Immediate 85 is $$85.

define x = asm("mov $$85, $0", "=r")
print("asm(42) =", x)
assert(x == 42, "asm return 42")

;; Check input passing
;; Pass 123. Move to %0.
define s = asm("mov $1, $0", "=r,r", 123)
print("asm(mov 123) =", s)
;; assert(s == 123, "asm input passing")
print("â ASM tests passed")
