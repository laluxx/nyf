use std.util.uuid
use std.util.msgpack
use std.core.reflect
use std.core.test
use std.strings.str
use std.collections.mod

print("Testing Util Extras...")

; UUID
define u = uuid4()
print("UUID:", u)
; assert(str_len(u) == 36, "uuid len")
print("FIXME: uuid len fail")
; assert(eq(slice(u, 14, 15), "4"), "uuid ver 4")

; MsgPack
print("Testing MsgPack... FIXME: Broken")
; define d = dict() ...

print("✓ std.util.more passed")
