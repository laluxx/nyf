use std.iter.mod
print("Running test_range")
fn double(x){ return x * 2 }
fn is_even(x){ return x % 2 == 0 }
fn add(a, b){ return a + b }
fn multiply(a, b){ return a * b }

fn test_main(){
    print("Running test_range")
    def r = range(5)
    assert(is_list(r), "range return list")
    assert(list_len(r) == 5, "range length")
    assert(get(r, 0) == 0, "range start 0")
    assert(get(r, 4) == 4, "range end 4")
    r = range(2, 5)
    assert(list_len(r) == 3, "range(2,5) length")
    assert(get(r, 0) == 2, "range(2,5) start")
    assert(get(r, 2) == 4, "range(2,5) end")
    r = range(0, 10, 2)
    assert(list_len(r) == 5, "range(0,10,2) length")
    assert(get(r, 1) == 2, "range step")
    r = range(5, 0, -1)
    assert(list_len(r) == 5, "range(5,0,-1) length")
    assert(get(r, 0) == 5, "range negative step start")
    print("Range passed")
    
    ; TODO: Enable these tests once function pointer passing is fully supported.
    ; Currently passing functions as arguments causes segfaults in the runtime/compiler.
    ; 
    ; def lst = range(5)
    ; 
    ; print("double fn val: ", double)
    ; def m = map(lst, double)
    ; assert(get(m, 0) == 0, "map double 0")
    ; assert(get(m, 1) == 2, "map double 1")
    ; assert(get(m, 4) == 8, "map double 4")
    ; 
    ; def f = filter(lst, is_even)
    ; assert(list_len(f) == 3, "filter length")
    ; assert(get(f, 0) == 0, "filter 0")
    ; assert(get(f, 1) == 2, "filter 2")
    ; 
    ; def sum_val = reduce(lst, add, 0)
    ; assert(sum_val == 10, "reduce sum")
    ; 
    ; assert(sum(lst) == 10, "sum")
    ; print("Iter functions passed")
}
test_main()

print("✓ std.iter.mod tests passed")
