use std.net.http
use std.core.test
use std.strings.str
use std.collections.mod
print("Testing HTTP...")
define part = http_parse_url("http://google.com/foo")
assert(eq(get(part, 0), "google.com"), "http parse host")
assert(get(part, 1) == 80, "http parse port")
assert(eq(get(part, 2), "/foo"), "http parse path")
define q = http_parse_query("a=1&b=2")
assert(eq(getitem(q, "a"), "1"), "query a")
assert(eq(getitem(q, "b"), "2"), "query b")
print("✓ std.net.http passed")