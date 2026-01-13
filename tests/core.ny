;;; core.ny --- Core Standard Library

;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: core

;; Function:  54
;; Variables: 2

;;; Commentary:

;; TODO Write Commentary

;;; Code:

use std.core.reflect
use std.strings.str; for itoa

;;; Memory Operations (aligned for speed)

fn bool(x) {
  "Converts a value to its boolean representation (0 is false, everything else is true)."
    return !!x
}

fn load8(p) {
  "Loads a single byte (uint8) from the specified memory address."
    return rt_load8(p)
}

fn load64(p) {
  "Loads an 8-byte integer (uint64) from the specified memory address."
    return rt_load64(p)
}

fn store8(p, v) {
  "Stores a single byte (uint8) to the specified memory address."
    return rt_store8(p, v)
}

fn memcpy(dst, src, n) {
    "Copies N bytes from SRC to DST."
    def i = 0
    while(i < n){
        store8(dst + i, load8(src + i))
        i = i + 1
    }
    return dst
}

fn store64(p, v) {
  "Stores an 8-byte integer (uint64) to the specified memory address."
    return rt_store64(p, v)
}

fn load16(p){
  "Loads a 2-byte integer (uint16) from the specified memory address using little-endian order."
    return load8(p) | (load8(p+1) << 8)
}

fn load32(p) {
  "Loads a 4-byte integer (uint32) from the specified memory address using little-endian order."
    return load8(p) | (load8(p+1) << 8) |
           (load8(p+2) << 16) | (load8(p+3) << 24)
}

fn store16(p, v) {
  "Stores a 2-byte integer (uint16) to the specified memory address using little-endian order."
    store8(p, v & 255)
    store8(p+1, (v >> 8) & 255)
    return v
}

fn store32(p, v) {
  "Stores a 4-byte integer (uint32) to the specified memory address using little-endian order."
    store8(p, v & 255)
    store8(p+1, (v >> 8) & 255)
    store8(p+2, (v >> 16) & 255)
    store8(p+3, (v >> 24) & 255)
    return v
}

fn malloc(n) {
  "Allocates N bytes of memory on the heap. Returns a pointer to the allocated memory."
    return rt_malloc(n)
}

fn free(p) {
  "Frees memory previously allocated with malloc or realloc."
    return rt_free(p)
}

fn realloc(p, newsz) {
  "Resizes the memory block pointed to by P to NEWSZ bytes."
    return rt_realloc(p, newsz)
}

;;; List Operations

;; List header: [TAG(8) | LEN(8) | CAP(8) | ...items]

fn list(cap=8) {
  "Creates a new empty dynamic list with initial capacity CAP."
    ;; Cap is tagged. Size = 24 + cap * 8 (tagged arithmetic handles it)

    def p = malloc(24 + cap * 8)
    store64(p, 100);     "L" tag

    ;; TODO Important
    ; def p = (malloc (* 8 (+ 24 cap)))
    ; def p = (quote (malloc (* 8 (+ 24 cap))))
    ; (store64 p 100);     "L" tag


    store64(p + 8, 0);    Count (raw 0)
    store64(p + 16, cap); Cap
    return p
}

fn ptr?(x){ return rt-ptr?(x) }
fn int?(x){ return rt-ptr?(x) == false }; heuristic

fn is_num(x) {
  "Checks if a value is a number (integer) rather than a pointer."
    if(rt_is_int(x)){ return true }
    return false
}

fn is_list(x) {
  "Checks if a value is a list."
    if(!rt_is_ptr(x)){ return false }
    def tag = load64(x)
    return tag == 100
}

fn is_dict(x) {
    "Checks if a value is a dictionary."
    if(!rt_is_ptr(x)){ return false }
    def tag = load64(x)
    return tag == 101
}

fn is_set(x) {
    "Checks if a value is a set."
    if(!rt_is_ptr(x)){ return false }
    def tag = load64(x)
    return tag == 102
}

fn is_tuple(x) {
  "Checks if a value is a tuple."
    if(!rt_is_ptr(x)){ return false }
    def tag = load64(x)
    return tag == 103
}

fn list_len(lst) {
    if(!rt_is_ptr(lst)){ return 0 }
    def tag = load64(lst)
    if(tag == 100 || tag == 101 || tag == 102 || tag == 103){
        def raw_len = load64(lst + 8)   ; Load raw (untagged) length
        return (raw_len << 1) | 1   ; Tag it before returning
    }
    return 0
}

fn list_clone(lst) {
  "Shallow-copies a list, preserving element order."
    if(lst == 0) {
        sys_write(1, "DBG: list_clone received null lst\n", 34)
        return 0
    }
    sys_write(1, "DBG: list_clone enter\n", 22)
    if(is_list(lst) == false){ return 0 }
    sys_write(1, "DBG: is_list ok\n", 16)
    def n = list_len(lst) >> 1 >> 1
    sys_write(1, "DBG: len ok\n", 12)
    out = list(8)
    sys_write(1, "DBG: list ok\n", 13)
    def i = 0
    while(i<n) {
        sys_write(1, "DBG: loop\n", 10)
        def val = get(lst, i)
        def out = append(out, val)
        i = i + 1
    }
    sys_write(1, "DBG: done\n", 10)
    return out
}

fn load_item(lst,i) {
  "Internal: Loads the i-th item from a collection's raw memory."
    def raw_i = i >> 1                    ; Untag index
    return load64(lst + (24 + raw_i * 8)) ; HDR_SIZE is 24
}

fn store_item(lst,i,v) {
  "Internal: Stores value V at the I-th position in a collection's raw memory."
    def raw_i = i >> 1  ; Untag index
    store64(lst + (24 + raw_i * 8), v)
    return v
}

fn get(obj,i) {
  "Retrieves the item at index I from a list, tuple, or dictionary.
For strings, returns a character substring."
    def t = type(obj)
    if(_str_eq(t, "str")){
        n = str_len(obj)
        if(i < 0){ i = i + n }
        if(i < 0 || i >= n){ return 0 }
        return slice(obj, i, i + 1)
    }
    if(is_dict(obj)){
        return getitem(obj, i, 0)
    }
    if(i < 0){
        i = i + list_len(obj)
    }
    return load_item(obj, i)
}

fn set_idx(obj,i,v) {
  "Sets the value at index I in a list or dictionary."
    if(is_dict(obj)) {
        return setitem(obj, i, v)
    }
    if(i < 0) {
        i = i + list_len(obj)
    }
    return store_item(obj, i, v)
}

fn append(lst, v) {
  "Appends value V to the end of list LST. Returns the list (may be reallocated)."
    if(rt_is_ptr(lst) == false){ return lst }
    def tag = load64(lst)
    def raw_len = load64(lst + 8)
    def raw_cap = load64(lst + 16)

    if(raw_len >= raw_cap) {
        def newcap = raw_cap * 2
        def newp = malloc(24 + newcap * 8)
        store64(newp + 0, tag)
        store64(newp + 8, raw_len)
        store64(newp + 16, newcap)

        def i = 0
        while(i<raw_len) {
            tagged_i = (i << 1) | 1
            store_item(newp, tagged_i, load_item(lst, tagged_i))
            i = i + 1
        }

        free(lst)
        lst = newp
    }

    store_item(lst, (raw_len << 1) | 1, v)
    store64(lst + 8, raw_len + 1)
    return lst
}

fn pop(lst) {
  "Removes and returns the last element from list LST."
    if(is_ptr(lst) == false){ return 0 }
    len = load64(lst + 8)
    if(len == 0){ return 0 }
    newlen = len - 1
    def v = load_item(lst, newlen)
    store64(lst + 8, newlen)
    return v
}

fn list_clear(lst) {
  "Removes all elements from the list LST."
    if(is_ptr(lst)) {
        store64(lst + 8, 1)
    }
    return lst
}

fn extend(lst, other) {
  "Extends list LST by appending all elements from list OTHER."
    if(is_list(lst) == false || is_list(other) == false){ return lst }
    def i = 0
    def n = list_len(other)
    while(i<n) {
        lst = append(lst, get(other, i))
        i = i + 1
    }
    return lst
}

;;; I/O Operations

define IO_BUF = 8192

fn sys_write(fd, buf, len){
  "Writes LEN bytes from BUF to file descriptor FD. System call."
    return rt_syscall(1, fd, buf, len, 0, 0, 0)
}

fn sys_read(fd, buf, n) {
  "Reads up to N bytes from file descriptor FD into BUF. System call."
    return rt_syscall(0, fd, buf, n, 0, 0, 0)
}

fn sys_open(path, flags, mode) {
  "Opens the file at PATH with specified FLAGS and MODE. System call."
    return rt_syscall(2, path, flags, mode, 0, 0, 0)
}

fn sys_close(fd) {
  "Closes the specified file descriptor. System call."
    return rt_syscall(3, fd, 0, 0, 0, 0, 0)
}

fn sys_stat(path, buf) {
  "Retrieves file status for PATH into BUF. System call."
    return rt_syscall(4, path, buf, 0, 0, 0, 0)
}

fn sys_fstat(fd, buf) {
  "Retrieves file status for open file descriptor FD into BUF. System call."
    return rt_syscall(5, fd, buf, 0, 0, 0, 0)
}

fn file_open(path, flags, mode) {
  "Opens a file and returns its file descriptor."
    return sys_open(path, flags, mode)
}

fn file_close(fd) {
  "Closes an open file descriptor."
    return sys_close(fd)
}

fn _str_eq(s1, s2) {
  "Compares two strings for equality."
    def n1 = str_len(s1)
    def n2 = str_len(s2)
    if(n1 != n2){ return false }
    def i = 0
    while(i<n1) {
        if(load8(s1 + i) != load8(s2 + i)){ return false }
        i = i + 1
    }
    return true
}

fn _to_string(v) {
  "Converts a value to its string representation."
    return rt_to_str(v)
}

fn _print_write(s) {
  "Internal: Writes a string to stdout without a newline."
    sys_write(1, s, str_len(s))
    return 0
}

fn kwarg(k, v) {
  "Internal: Creates a keyword argument wrapper."
    def p = malloc(24) ; Tag(8) + Key(8) + Val(8)
    store64(p, 104)    ; Tag 104 for Kwarg
    store64(p + 8, k)
    store64(p + 16, v)
    return p
}

fn is_kwarg(x) {
    if(!rt_is_ptr(x)){ return false }
    return load64(x) == 104
}

fn get_kwarg_key(x) {
    return load64(x + 8)
}

fn get_kwarg_val(x) {
    return load64(x + 16)
}

fn print(...args) {
    def end = "\n"
    def delim = " "

    def n = list_len(args) >> 1 >> 1
    def i = 0
    def cnt = 0

    ; First pass: identify kwargs and count print items
    while(i<n) {
        item = get(args, i)
        if(is_kwarg(item)){
            def k = get_kwarg_key(item)
            def v = get_kwarg_val(item)
            if(_str_eq(k, "end")){ end = v }
            if(_str_eq(k, "delim")){ delim = v }
        } else {
             cnt = cnt + 1
        }
        i = i + 1
    }

    ; Second pass: print items
    i = 0
    def printed = 0
    while(i < n){
        def item = get(args, i)
        if(is_kwarg(item) == false){
            def s = _to_string(item)
            sys_write(1, s, str_len(s))
            printed = printed + 1
            if(printed < cnt){
                sys_write(1, delim, str_len(delim))
            }
        }
        i = i + 1
    }
    sys_write(1, end, str_len(end))
    return 0
}


fn input(prompt) {
  "Displays PROMPT and reads a line of input from stdin. Returns the input as a string."
    if(prompt != 0) {
        sys_write(1, prompt, str_len(prompt))
    }

    def cap = IO_BUF
    def buf = malloc(cap)
    def pos = 0

    while(1) {
        n = sys_read(0, buf + pos, cap - pos - 1)
        if(n <= 0){ break  }

        i = 0
        while(i < n){
            if(load8(buf + pos + i) == 10){ ; Newline character
                pos = pos + i
                store8(buf + pos, 0) ; Null-terminate
                return buf
            }
            i = i + 1
        }

        pos = pos + n
        if(pos + 1 >= cap){
            newcap = cap * 2
            nb = malloc(newcap)
            memcpy(nb, buf, cap)
            free(buf)
            buf = nb
            cap = newcap
        }
    }

    store8(buf + pos, 0) ; Null-terminate even if no newline
    return buf
}

fn file_read(path) {
  "Reads the entire content of a file into a string."
    def fd = sys_open(path, 0, 0); O_RDONLY
    if(fd<0) {
        return ""
    }

    def cap = IO_BUF
    def buf = malloc(cap)
    def pos = 0

    while(1) {
        def n = sys_read(fd, buf + pos, cap - pos - 1)
        if(n<=0) {
            break
        }
        pos = pos + n
        if(pos + 1 >= cap){
            def newcap = cap * 2
            def nb = malloc(newcap)
            memcpy(nb, buf, pos)
            free(buf)
            buf = nb
            cap = newcap
        }
    }

    sys_close(fd)
    store8(buf + pos, 0); Null-terminate
    return buf
}

fn file_write(path, data) {
  "Writes string DATA to the file at PATH, overwriting any existing content."
    def fd = sys_open(path, 577, 420); O_WRONLY | O_CREAT | O_TRUNC (flags 0o2201, mode 0o644)
    if(fd < 0){ return -1 }
    def n = sys_write(fd, data, str_len(data))
    sys_close(fd)
    return n
}

fn file_append(path, data){
  "Appends string DATA to the end of the file at PATH."
    def fd = sys_open(path, 1089, 420); O_WRONLY | O_CREAT | O_APPEND (flags 0o4201, mode 0o644)
    if(fd < 0){ return -1 }
    def n = sys_write(fd, data, str_len(data))
    sys_close(fd)
    return n
}

fn file_exists(path) {
  "Checks if a file or directory exists at the specified path."
    def buf = malloc(144); size of struct stat
    def r = sys_stat(path, buf)
    free(buf)
    return r == 0
}

fn file_remove(path) {
  "Deletes the file at the specified path. System call."
    return rt_syscall(87, path, 0, 0, 0, 0, 0); unlink
}

fn cwd() {
  "Returns the current working directory as a string."
    def buf = malloc(4096)
    def n = rt_syscall(79, buf, 4096, 0, 0, 0, 0); getcwd
    if(n < 0){ free(buf)  return "" }
    store8(buf + n, 0)
    return buf
}

;;; Initialize global symbol table

define _globals = dict(17)
rt_set_globals(_globals)
