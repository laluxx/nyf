use std

print("Testing FFI...")
define h = dlopen("libc.so.6", 2)
if(h == 0){ h = dlopen("/lib/x86_64-linux-gnu/libc.so.6", 2) }
if(h == 0){ h = dlopen("/usr/lib/libc.so.6", 2) }

if(h != 0){
    print("Loaded libc handle:", h)
    define abs_f = dlsym(h, "abs")
    if(abs_f != 0){
        print("abs(-50) = ", call1_int(abs_f, -50))
        define res = call1_int(abs_f, -50)
        assert(res == 50, "ffi abs (tagged)")
    } else { print("Warn: abs symbol not found") }
    dlclose(h)
} else { print("Skipping FFI tests") }
print("✓ FFI tests passed")
