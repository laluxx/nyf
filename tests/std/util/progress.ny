use std.util.progress
use std.core.reflect
use std.core.test
use std.core.core
use std.collections.mod

print("Testing Util Progress...")
print("Testing Util Progress...")

define xs = [1, 2, 3]
fn f(x){ return x*2 }

define res = progress_map(f, xs, "Test Map")
print("LEN:", len(res))
print("RES:", res)
assert(len(res) == 3, "progress_map len")
assert(get(res, 0) == 2, "map val")

define p = progress(10, "Manual")
progress_update(p, 5)
progress_finish(p)
print("✓ std.util.progress passed")
