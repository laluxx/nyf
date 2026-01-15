; Test std.collections.mod - Collection operations
use std.collections.mod

; Dict operations
d = dict(8)
assert(is_dict(d), "dict creation")
d = setitem(d, "name", "Nytrix")
d = setitem(d, "version", "0.1")
d = setitem(d, "year", 2026)
assert(eq(getitem(d, "name", 0), "Nytrix"), "get string value")
assert(getitem(d, "year", 0) == 2026, "get int value")
assert(has(d, "name"), "dict has key")
assert(!has(d, "missing"), "dict doesn't have key")
its = items(d)
assert(len(its) == 3, "items returns all pairs")
ks = keys(d)
assert(len(ks) == 3, "keys returns all keys")
vs = values(d)
assert(len(vs) == 3, "values returns all values")

; Set operations
s = set()
assert(is_set(s), "set creation")
s = add(s, 1)
s = add(s, 2)
s = add(s, 3)
s = add(s, 2)
assert(set_contains(s, 1), "set contains 1")
assert(set_contains(s, 2), "set contains 2")
assert(!set_contains(s, 10), "set doesn't contain 10")
s = remove(s, 2)
assert(!set_contains(s, 2), "element removed")

; List helpers
lst = [3, 1, 4, 1, 5, 9, 2, 6]
assert(list_contains(lst, 4), "list contains element")
assert(!list_contains(lst, 10), "list doesn't contain element")
rev = list_reversed(lst)
assert(get(rev, 0) == 6, "reversed first element")
assert(get(rev, -1) == 3, "reversed last element")

; Sorted
sorted_lst = sorted(lst)
assert(get(sorted_lst, 0) == 1, "sorted first element")
assert(get(sorted_lst, -1) == 9, "sorted last element")
i = 0
while(i < len(sorted_lst) - 1){
    assert(get(sorted_lst, i) <= get(sorted_lst, i + 1), "ascending order")
    i = i + 1
}

print("✓ std.collections.mod tests passed")
