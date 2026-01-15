use std.collections.queue
use std.core.error

fn test_queue(){
    print("Testing queue...")
    def q = queue()
    assert(qlen(q) == 0, "size 0")
    
    enqueue(q, 10)
    enqueue(q, 20)
    enqueue(q, 30)
    assert(qlen(q) == 3, "size 3")
    
    assert(dequeue(q) == 10, "dequeue 10")
    assert(dequeue(q) == 20, "dequeue 20")
    assert(qlen(q) == 1, "size 1")
    
    enqueue(q, 40)
    assert(dequeue(q) == 30, "dequeue 30")
    assert(dequeue(q) == 40, "dequeue 40")
    assert(qlen(q) == 0, "empty")
    assert(dequeue(q) == 0, "dequeue empty")
    
    print("Queue passed")
}

fn test_main(){
    test_queue()
    print("✓ std.collections.queue passed")
}

test_main()
