use std.collections.set
use std.collections.dict ; for iterators on set items if needed (items())
use std.core.error

fn test_basic(){
    print("Testing set basic operations...")
    def s = set()
    s = add(s, 10)
    s = add(s, 20)
    
    assert(set_contains(s, 10), "Contains 10")
    assert(set_contains(s, 20), "Contains 20")
    assert(!set_contains(s, 30), "Does not contain 30")
    
    s = remove(s, 10)
    assert(!set_contains(s, 10), "Removed 10")
    assert(set_contains(s, 20), "Still has 20")
    
    print("Basic operations passed")
}

fn test_ops(){
    print("Testing set union/intersection/difference...")
    def s1 = set()
    add(s1, 1)
    add(s1, 2)
    
    def s2 = set()
    add(s2, 2)
    add(s2, 3)
    
    def u = set_union(s1, s2)
    assert(set_contains(u, 1), "Union has 1")
    assert(set_contains(u, 2), "Union has 2")
    assert(set_contains(u, 3), "Union has 3")
    
    def i = set_intersection(s1, s2)
    assert(!set_contains(i, 1), "Intersection no 1")
    assert(set_contains(i, 2), "Intersection has 2")
    assert(!set_contains(i, 3), "Intersection no 3")
    
    print("DEBUG: s2 items:")
    def d = set_difference(s1, s2)
    assert(set_contains(d, 1), "Diff has 1")
    assert(!set_contains(d, 2), "Diff no 2")
    assert(!set_contains(d, 3), "Diff no 3")
    
    print("Set ops passed")
}

fn test_main(){
    test_basic()
    test_ops()
    print("✓ std.collections.set passed")
}

test_main()
