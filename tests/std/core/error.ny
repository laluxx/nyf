; Test std.core.error - Error handling and assertions
use std.core.error

; Assert success
assert(true, "true assertion")
assert(1 == 1, "equality assertion")
assert(5 > 3, "comparison assertion")

; AssertEqual
assert_eq(42, 42, "integers equal")
assert_eq("hello", "hello", "strings equal")
assert_eq([1, 2, 3], [1, 2, 3], "lists equal")
print("✓ std.core.error tests passed")
