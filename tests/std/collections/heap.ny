use std.collections.heap
use std.core.error

fn test_heap(){
    print("Testing heap...")
    def h = heap()
    hpush(h, 50)
    hpush(h, 30)
    hpush(h, 40)
    hpush(h, 10)
    hpush(h, 20)
    
    assert(hpeek(h) == 10, "peek min 10")
    
    assert(hpop(h) == 10, "pop 10")
    assert(hpop(h) == 20, "pop 20")
    assert(hpop(h) == 30, "pop 30")
    assert(hpop(h) == 40, "pop 40")
    assert(hpop(h) == 50, "pop 50")
    
    assert(hpop(h) == 0, "pop empty")
    
    print("Heap passed")
}

fn test_main(){
    test_heap()
    print("✓ std.collections.heap passed")
}

test_main()
