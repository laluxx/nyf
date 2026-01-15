use std.collections.mod

l = [1, 2, 3, 4, 5]
rev = list_reversed(l)
assert(len(rev) == 5, "Reversed list should have the same length")
assert(get(rev, 0) == 5, "First element of reversed list is wrong")
assert(get(rev, 1) == 4, "Second element of reversed list is wrong")
assert(get(rev, 2) == 3, "Third element of reversed list is wrong")
assert(get(rev, 3) == 2, "Fourth element of reversed list is wrong")
assert(get(rev, 4) == 1, "Fifth element of reversed list is wrong")
print("✓ list_reversed tests passed")
