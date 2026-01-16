use std.core.mod
use std.core.reflect
define DICT_MAGIC = 101
define STATE_EMPTY = 0
define STATE_OCCUPIED = 1
define STATE_DELETED = 2
define ENTRY_SIZE = 24
define HEADER_SIZE = 16
fn dict(cap = 8) {
    "Create a new empty hash-table dictionary."
    if cap < 8 {
        cap = 8
    }
    def c = 8
    while c < cap {
        c = c * 2
    }
    cap = c
    def sz = 16 + cap * 24
    def d = rt_malloc(sz)
    store64(d, DICT_MAGIC, -8)
    store64(d, 0, 0)
    store64(d, cap, 8)
    def i = 0
    while i < cap {
        store64(d, 0, 16 + i * 24 + 16)
        i = i + 1
    }
    return d
}

fn dict_resize(d, new_cap) {
    def old_cap = load64(d, 8)
    def new_d = dict(new_cap)
    def i = 0
    while i < old_cap {
        def off = 16 + i * 24
        if load64(d, off + 16) == 1 {
            def new_val = load64(d, off + 8)
            new_d = setitem(new_d, load64(d, off), new_val)
        }
        i = i + 1
    }
    return new_d
}

fn setitem(d, key, val) {
    "Sets the value for a given key in dictionary d."
    def count = load64(d, 0)
    def cap = load64(d, 8)
    if count * 2 >= cap {
        d = dict_resize(d, cap * 2)
        count = load64(d, 0)
        cap = load64(d, 8)
    }
    def h = hash(key)
    def mask = cap - 1
    def idx = h & mask
    def perturb = h
    def probes = 0
    def first_free = -1
    while probes < cap {
        def off = 16 + idx * 24
        def st = load64(d, off + 16)
        if st == STATE_EMPTY {
            if first_free != -1 {
                off = first_free
            }
            store64(d, key, off)
            store64(d, val, off + 8)
            store64(d, STATE_OCCUPIED, off + 16)
            store64(d, count + 1, 0)
            return d
        }
        if st == STATE_DELETED {
            if first_free == -1 {
                first_free = off
            }
        }
        if st == STATE_OCCUPIED {
            if eq(load64(d, off), key) {
                store64(d, val, off + 8)
                return d
            }
        }
        idx = idx * 5 + 1 + perturb >> 5 & mask
        perturb = perturb >> 5
        probes = probes + 1
    }
    if first_free != -1 {
        def off = first_free
        store64(d, key, off)
        store64(d, val, off + 8)
        store64(d, STATE_OCCUPIED, off + 16)
        store64(d, count + 1, 0)
        return d
    }
    return d
}

fn getitem(d, key, default_val) {
    "Gets the value for key in dictionary d, or returns default_val if not found."
    def cap = load64(d, 8)
    def h = hash(key)
    def mask = cap - 1
    def idx = h & mask
    def perturb = h
    def probes = 0
    while probes < cap {
        def off = 16 + idx * 24
        def st = load64(d, off + 16)
        if st == STATE_EMPTY {
            return default_val
        }
        if st == STATE_OCCUPIED {
            if eq(load64(d, off), key) {
                return load64(d, off + 8)
            }
        }
        idx = idx * 5 + 1 + perturb >> 5 & mask
        perturb = perturb >> 5
        probes = probes + 1
    }
    return default_val
}

fn has(d, key) {
    "Return true if key exists in dictionary d, false otherwise."
    def cap = load64(d, 8)
    def h = hash(key)
    def mask = cap - 1
    def idx = h & mask
    def perturb = h
    def probes = 0
    while probes < cap {
        def off = 16 + idx * 24
        def st = load64(d, off + 16)
        if st == STATE_EMPTY {
            return false
        }
        if st == STATE_OCCUPIED {
            if eq(load64(d, off), key) {
                return true
            }
        }
        idx = idx * 5 + 1 + perturb >> 5 & mask
        perturb = perturb >> 5
        probes = probes + 1
    }
    return false
}

fn delitem(d, key) {
    "Removes key from dictionary d if present."
    def cap = load64(d, 8)
    def h = hash(key)
    def mask = cap - 1
    def idx = h & mask
    def perturb = h
    def probes = 0
    while probes < cap {
        def off = 16 + idx * 24
        def st = load64(d, off + 16)
        if st == STATE_EMPTY {
            return d
        }
        if st == STATE_OCCUPIED {
            if eq(load64(d, off), key) {
                store64(d, STATE_DELETED, off + 16)
                store64(d, load64(d, 0) - 1, 0)
                return d
            }
        }
        idx = idx * 5 + 1 + perturb >> 5 & mask
        perturb = perturb >> 5
        probes = probes + 1
    }
    return d
}

fn items(d) {
    "Return a list of all [key, value] pairs in dictionary d."
    def res = list(8)
    def cap = load64(d, 8)
    def i = 0
    while i < cap {
        def off = 16 + i * 24
        if load64(d, off + 16) == STATE_OCCUPIED {
            res = append(res, [load64(d, off), load64(d, off + 8)])
        }
        i = i + 1
    }
    return res
}

fn keys(d) {
    "Return a list of all keys in dictionary d."
    def res = list(8)
    def cap = load64(d, 8)
    def i = 0
    while i < cap {
        def off = 16 + i * 24
        if load64(d, off + 16) == STATE_OCCUPIED {
            res = append(res, load64(d, off))
        }
        i = i + 1
    }
    return res
}

fn values(d) {
    "Return a list of all values in dictionary d."
    def res = list(8)
    def cap = load64(d, 8)
    def i = 0
    while i < cap {
        def off = 16 + i * 24
        if load64(d, off + 16) == STATE_OCCUPIED {
            res = append(res, load64(d, off + 8))
        }
        i = i + 1
    }
    return res
}

fn dict_clear(d) {
    "Removes all items from dictionary d."
    def cap = load64(d, 8)
    def i = 0
    while i < cap {
        store64(d, 0, 16 + i * 24 + 16)
        i = i + 1
    }
    store64(d, 0, 0)
    return d
}

fn dict_copy(d) {
    "Return a shallow copy of dictionary d."
    def cap = load64(d, 8)
    def out = dict(cap)
    def i = 0
    while i < cap {
        def off = 16 + i * 24
        if load64(d, off + 16) == 1 {
            out = setitem(out, load64(d, off), load64(d, off + 8))
        }
        i = i + 1
    }
    return out
}

fn dict_update(d, other) {
    "Updates dictionary d with items from other. Other can be another dict or a list of pairs."
    if is_dict(other) {
        def its = items(other)
        def i = 0
        def n = list_len(its)
        while i < n {
            def p = get(its, i)
            d = setitem(d, p[0], p[1])
            i = i + 1
        }
    } else {
        def i = 0
        def n = list_len(other)
        while i < n {
            def p = get(other, i)
            d = setitem(d, p[0], p[1])
            i = i + 1
        }
    }
    return d
}

fn is_dict(d) {
    if !rt_is_ptr(d) {
        return 0
    }
    return load64(d, -8) == DICT_MAGIC
}
