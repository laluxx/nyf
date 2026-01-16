use std.core.mod
use std.math.random
use std.io
fn uuid4() {
	"Generate UUIDv4-like string."
	def out = rt_malloc(64)
	store64(out, 120, -8)
	def i = 0
	def o = 0
	def r = 0
	def b = 0
	def v1 = 0
	def c1 = 255
	def v2 = 0
	def c2 = 255
	while i < 16 {
		r = rand()
		def b = r % 256
		if i == 6 {
			b = b % 16 + 64
		}
		if i == 8 {
			b = b % 64 + 128
		}
		v1 = b / 16 % 16
		if v1 < 10 {
			c1 = 48 + v1
		} else {
			c1 = 87 + v1
		}
		v2 = b % 16
		if v2 < 10 {
			c2 = 48 + v2
		} else {
			c2 = 87 + v2
		}
		store8(out, c1, o)
		store8(out, c2, o + 1)
		print("DEBUG: o=", o, " c1=", c1, " read=", rt_load8_idx(out, o))
		print("DEBUG: o+1=", o+1, " c2=", c2, " read=", rt_load8_idx(out, o+1))
		o = o + 2
		if i == 3 || i == 5 || i == 7 || i == 9 {
			store8(out, 45, o)
			o = o+1
		}
		i = i+1
	}
	store8(out, 0, o)
	return out
}
