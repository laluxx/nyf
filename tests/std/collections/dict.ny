use std.collections.dict
use std.core.error

fn test_basic(){
    print("Testing dict basic operations...")
    def d = dict()
    assert(load64(d) == 0, "Initial count 0")
    
    d = setitem(d, "key1", 100)
    assert(load64(d) == 1, "Count 1 after insert")
    assert(getitem(d, "key1", 0) == 100, "Get existing key")
    assert(getitem(d, "missing", 999) == 999, "Get missing key returns default")
    assert(has(d, "key1"), "Has existing key")
    assert(!has(d, "missing"), "Has missing key")
    
    d = setitem(d, "key1", 200)
    assert(getitem(d, "key1", 0) == 200, "Update existing key")
    assert(load64(d) == 1, "Count remains 1 after update")
    
    d = setitem(d, "key2", 300)
    assert(load64(d) == 2, "Count 2 after second insert")
    assert(getitem(d, "key2", 0) == 300, "Get second key")
    
    d = delitem(d, "key1")
    assert(load64(d) == 1, "Count 1 after delete")
    assert(!has(d, "key1"), "Deleted key gone")
    assert(has(d, "key2"), "Other key remains")
    
    print("Basic operations passed")
}

fn test_resize(){
    print("Testing dict resizing...")
    def d = dict(8)
    def i = 0
    while(i < 20){
        d = setitem(d, i, i * 10)
        i = i + 1
    }
    
    assert(load64(d) == 20, "Count correct after many inserts")
    
    i = 0
    while(i < 20){
        assert(getitem(d, i, -1) == i * 10, "Get value after resize")
        i = i + 1
    }
    print("Resize passed")
}

fn test_methods(){
    print("Testing dict methods (keys, values, items)...")
    def d = dict()
    d = setitem(d, "a", 1)
    d = setitem(d, "b", 2)
    
    def k = keys(d)
    assert(list_len(k) == 2, "keys length")
    ; Order isn't guaranteed, but check membership implicitly via count
    
    def v = values(d)
    assert(list_len(v) == 2, "values length")
    
    def it = items(d)
    assert(list_len(it) == 2, "items length")
    
    print("Methods passed")
}

fn test_copy_update(){
    print("Testing dict copy and update...")
    def d1 = dict()
    setitem(d1, "a", 1)
    
    def d2 = dict_copy(d1)
    assert(getitem(d2, "a", 0) == 1, "Copy has item")
    setitem(d2, "b", 2)
    assert(!has(d1, "b"), "Original unmodified by copy modification")
    
    def d3 = dict()
    setitem(d3, "c", 3)
    dict_update(d1, d3)
    assert(getitem(d1, "c", 0) == 3, "Update from dict")
    
    print("Copy/Update passed")
}

fn test_main(){
    test_basic()
    test_resize()
    test_methods()
    test_copy_update()
    print("✓ std.collections.dict passed")
}

test_main()
