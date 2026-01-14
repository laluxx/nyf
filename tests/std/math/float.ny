
use std.math.float

assert(feq(fadd(float(1), float(2)), float(3)), "float add")
assert(feq(fsub(float(3), float(2)), float(1)), "float sub")
assert(feq(fmul(float(2), float(3)), float(6)), "float mul")
assert(feq(fdiv(float(6), float(2)), float(3)), "float div")

assert(flt(float(1), float(2)), "float lt")
assert(fgt(float(2), float(1)), "float gt")

assert(floor(float(1)) == 1, "floor int")
assert(floor(fadd(float(1), float(0))) == 1, "floor 1.0")

define f3 = float(3)
define f2 = float(2)
define f1_5 = fdiv(f3, f2)
assert(floor(f1_5) == 1, "floor 1.5")
assert(ceil(f1_5) == 2, "ceil 1.5")

define f0 = float(0)
define fn1_5 = fsub(f0, f1_5) ; -1.5
assert(floor(fn1_5) == -2, "floor -1.5")
assert(ceil(fn1_5) == -1, "ceil -1.5")

print("✓ std.math.float tests passed")
