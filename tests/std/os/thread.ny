use std.os.thread
use std.core.test

print("Testing Threads...")

; Alloc shared counter
define counter_ptr = rt_malloc(8)
store64(counter_ptr, 0)
define mtx = mutex_new()

define ptr_val = load64(counter_ptr)
assert(ptr_val == 0, "init counter")

; Define thread function
; Must avoid complex closures if runtime doesn't support them fully in threads?
; Nytrix functions are usually code pointers.
; We can't pass 'mtx' and 'counter_ptr' easily if they are not in 'arg'.
; 'arg' is single int64.
; We can pack [mtx, counter_ptr] into a list/tuple (pointer).
; List is pointer.

fn worker(args){
    def m = load64(args)          ; list[0] is array pointer? No, list impl details.
    ; Wait, proper list access?
    ; 'args' is the pointer to the List struct?
    ; If we pass a List, it's a pointer.
    ; But accessing it inside thread involves GC/Allocator safety?
    ; Assuming read-only access to 'args' structure is safe.

    use std.collections.mod
    def m = get(args, 0)
    def c = get(args, 1)

    mutex_lock(m)
    def v = load64(c)
    store64(c, v + 1)
    mutex_unlock(m)
    return 0
}

define args = list()
args = append(args, mtx)
args = append(args, counter_ptr)

define t1 = thread_spawn(worker, args)
define t2 = thread_spawn(worker, args)
define t3 = thread_spawn(worker, args)

thread_join(t1)
thread_join(t2)
thread_join(t3)

define final = load64(counter_ptr)
print("Final counter:", final)
assert(final == 3, "thread mutex counter")

mutex_free(mtx)
rt_free(counter_ptr)

print("✓ std.os.thread passed")
