use std.core.reflect
use std.strings.str
fn bool(x) {
	"Convert a value to its boolean representation."
	return !!x
}

fn load8(p, i = 0) {
	"Load a single byte from address `p + i`."
	if i == 0 {
		return rt_load8(p)
	}
	return rt_load8_idx(p, i)
}

fn load64(p, i = 0) {
	"Load an 8-byte integer (uint64) from address `p + i`."
	if i == 0 {
		return rt_load64(p)
	}
	return rt_load64_idx(p, i)
}

fn store8(p, v, i = 0) {
	"Store byte `v` at address `p + i`."
	if i == 0 {
		return rt_store8(p, v)
	}
	return rt_store8_idx(p, i, v)
}

fn memcpy(dst, src, n) {
	"Copies `n` bytes from `src` to `dst`."
	return rt_memcpy(dst, src, n)
}

fn memset(dst, v, n) {
	"Memory sets `n` bytes of `dst` to `v`."
	return rt_memset(dst, v, n)
}

fn memcmp(a, b, n) {
	"Compares `n` bytes from `a` to `b` and returns the result."
	return rt_memcmp(a, b, n)
}

fn store64(p, v, i = 0) {
	"Store an 8-byte integer (uint64) to address `p + i`."
	if i == 0 {
		return rt_store64(p, v)
	}
	return rt_store64_idx(p, i, v)
}

fn load16(p, i = 0) {
	"Load a 2-byte integer (uint16) from address `p + i` using little-endian order."
	if i == 0 {
		return rt_load16(p)
	}
	return rt_load16_idx(p, i)
}

fn load32(p, i = 0) {
	"Load a 4-byte integer (uint32) from address `p + i` using little-endian order."
	if i == 0 {
		return rt_load32(p)
	}
	return rt_load32_idx(p, i)
}

fn store16(p, v, i = 0) {
	"Store a 2-byte integer (uint16) at address `p + i` using little-endian order."
	if i == 0 {
		return rt_store16(p, v)
	}
	return rt_store16_idx(p, i, v)
}

fn store32(p, v, i = 0) {
	"Store a 4-byte integer (uint32) at address `p + i` using little-endian order."
	if i == 0 {
		return rt_store32(p, v)
	}
	return rt_store32_idx(p, i, v)
}

fn ptr_add(p, n) {
	"Add an integer offset `n` to a pointer `p`, correctly handling tagged integers."
	return rt_ptr_add(p, n)
}

fn ptr_sub(p, n) {
	"Subtract an integer offset `n` from pointer `p`."
	return rt_ptr_sub(p, n)
}

fn malloc(n) {
	"Allocates `n` bytes of memory on the heap. Returns a pointer to the allocated memory."
	return rt_malloc(n)
}

fn free(p) {
	"Frees memory previously allocated with malloc or realloc."
	return rt_free(p)
}

fn realloc(p, newsz) {
	"Resizes the memory block pointed to by `p` to `newsz` bytes."
	return rt_realloc(p, newsz)
}

fn list(cap = 8) {
	"Create a new empty dynamic list with initial capacity `cap`."
	def p = rt_malloc(16 + cap * 8)
	if p == 0 {
		panic("list malloc failed")
	}
	rt_store64_idx(p, -8, 100)
	rt_store64_idx(p, 0, 0)
	rt_store64_idx(p, 8, cap)
	return p
}

fn is_ptr(x) {
	return rt_is_ptr(x)
}

fn is_int(x) {
	return rt_is_int(x)
}

fn is_num(x) {
	"Check if a value is a number (integer)."
	return is_int(x)
}

fn is_nytrix_obj(x) {
	"Check if a value is a valid Nytrix object pointer (must be aligned and > 4KB)."
	if rt_is_ptr(x) == false {
		return false
	}
	if x & 7 != 0 {
		return false
	}
	return true
}

fn is_list(x) {
	"Check if a value is a list."
	if is_int(x) {
		return false
	}
	if !is_nytrix_obj(x) {
		return false
	}
	return rt_load64_idx(x, -8) == 100
}

fn is_dict(x) {
	"Check if a value is a dictionary."
	if is_int(x) {
		return false
	}
	if !is_nytrix_obj(x) {
		return false
	}
	return rt_load64_idx(x, -8) == 101
}

fn is_set(x) {
	"Check if a value is a set."
	if is_int(x) {
		return false
	}
	if !is_nytrix_obj(x) {
		return false
	}
	return rt_load64_idx(x, -8) == 102
}

fn is_tuple(x) {
	"Check if a value is a tuple."
	if is_int(x) {
		return false
	}
	if !is_nytrix_obj(x) {
		return false
	}
	def tag = rt_load64_idx(x, -8)
	return tag == 103
}

fn is_str(x) {
	"Check if a value is a string."
	return rt_is_str(x)
}

fn is_kwargs(x) {
	"Check if a value is a keyword argument wrapper."
	if !is_nytrix_obj(x) {
		return false
	}
	return load64(x, -8) == 104
}

fn kwarg(k, v) {
	"Internal: Creates a keyword argument wrapper."
	def p = rt_malloc(16)
	store64(p, 104, -8)
	store64(p, k, 0)
	store64(p, v, 8)
	return p
}

fn is_kwarg(x) {
	if !rt_is_ptr(x) {
		return false
	}
	return load64(x, -8) == 104
}

fn get_kwarg_key(x) {
	return rt_load64_idx(x, 0)
}

fn get_kwarg_val(x) {
	return rt_load64_idx(x, 8)
}

fn list_len(lst) {
	if !rt_is_ptr(lst) {
		return 0
	}
	def tag = rt_load64_idx(lst, -8)
	if tag >= 100 {
		if tag <= 103 {
			return rt_load64_idx(lst, 0)
		}
	}
	return 0
}

fn list_clone(lst) {
	"Shallow-copies a list, preserving element order."
	if lst == 0 {
		return 0
	}
	if is_list(lst) == false {
		return 0
	}
	def n = list_len(lst)
	def out = list(n)
	def i = 0
	while i < n {
		def val = get(lst, i)
		out = append(out, val)
		i = i + 1
	}
	return out
}

fn load_item(lst, i) {
	"Internal: Loads the i-th item from a collection's raw memory."
	def offset = 16 + i * 8
	return rt_load64_idx(lst, offset)
}

fn store_item(lst, i, v) {
	"Internal: Stores value `v` at the i-th position in a collection's raw memory."
	def offset = 16 + i * 8
	rt_store64_idx(lst, offset, v)
	return v
}

fn get(obj, i) {
	"Retrieves the item at index `i` from a list, tuple,
or dictionary.  For strings, returns a character substring."
	if is_str(obj) {
		def n = str_len(obj)
		if i < 0 {
			i = i + n
		}
		if i < 0 {
			return 0
		}
		if i >= n {
			return 0
		}
		return slice(obj, i, i + 1)
	}
	if is_dict(obj) {
		return getitem(obj, i)
	}
	if is_list(obj) || is_tuple(obj) {
		def n = list_len(obj)
		if i < 0 {
			i = i + n
		}
		if i < 0 {
			return 0
		}
		if i >= n {
			return 0
		}
		return load_item(obj, i)
	}
	return 0
}

fn set_idx(obj, i, v) {
	"Sets the value at index `i` in a list or dictionary."
	if is_dict(obj) {
		return setitem(obj, i, v)
	}
	if is_list(obj) {
		def n = list_len(obj)
		if i < 0 {
			i = i + n
		}
		if i < 0 {
			return 0
		}
		if i >= n {
			return 0
		}
		return store_item(obj, i, v)
	}
	return 0
}

fn append(lst, v) {
	"Appends value `v` to the end of list `lst`. Returns the list (may be reallocated)."
	if is_nytrix_obj(lst) == false {
		return lst
	}
	def tag = rt_load64_idx(lst, -8)
	def n = rt_load64_idx(lst, 0)
	def cap = rt_load64_idx(lst, 8)
	if n >= cap {
		def newcap = cap * 2
		if newcap == 0 {
			newcap = 8
		}
		def newp = list(newcap)
		rt_store64_idx(newp, -8, tag)
		def i = 0
		while i < n {
			store_item(newp, i, load_item(lst, i))
			i = i + 1
		}
		rt_free(lst)
		lst = newp
		cap = newcap
	}
	store_item(lst, n, v)
	rt_store64_idx(lst, 0, n + 1)
	return lst
}

fn pop(lst) {
	"Removes and returns the last element from list `lst`."
	if is_ptr(lst) == false {
		return 0
	}
	def n = rt_load64_idx(lst, 0)
	if n == 0 {
		return 0
	}
	def newlen = n - 1
	def v = load_item(lst, newlen)
	store64(lst, newlen, 0)
	return v
}

fn list_clear(lst) {
	"Removes all elements from the list `lst`."
	if is_ptr(lst) {
		store64(lst, 0, 0)
	}
	return lst
}

fn extend(lst, other) {
	"Extends list `lst` by appending all elements from list `other`."
	if is_list(lst) == false {
		return lst
	}
	if is_list(other) == false {
		return lst
	}
	def i = 0
	def n = list_len(other)
	while i < n {
		lst = append(lst, get(other, i))
		i = i + 1
	}
	return lst
}
define IO_BUF = 8192
fn sys_write(fd, buf, n, i = 0) {
	"Writes `n` bytes from buffer `buf + i` to file descriptor `fd`."
	if i == 0 {
		return rt_syscall(1, fd, buf, n, 0, 0, 0)
	}
	return rt_sys_write_off(fd, buf, n, i)
}

fn sys_read(fd, buf, n, i = 0) {
	"Reads `n` bytes from file descriptor `fd` into buffer `buf + i`."
	if i == 0 {
		return rt_syscall(0, fd, buf, n, 0, 0, 0)
	}
	return rt_sys_read_off(fd, buf, n, i)
}

fn sys_open(path, flags, mode) {
	"Opens the file at `path` with specified `flags` and `mode`. System call."
	return rt_syscall(2, path, flags, mode, 0, 0, 0)
}

fn sys_close(fd) {
	"Closes the specified file descriptor. System call."
	return rt_syscall(3, fd, 0, 0, 0, 0, 0)
}

fn sys_stat(path, buf) {
	"Retrieves file status for `path` into `buf`. System call."
	return rt_syscall(4, path, buf, 0, 0, 0, 0)
}

fn sys_fstat(fd, buf) {
	"Retrieves file status for open file descriptor `fd` into `buf`. System call."
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
	if n1 != n2 {
		return false
	}
	def i = 0
	while i < n1 {
		if load8(s1, i) != load8(s2, i) {
			return false
		}
		i = i + 1
	}
	return true
}

fn _to_string(v) {
	if is_list(v) {
		def n = list_len(v)
		def s = "["
		def i = 0
		while i < n {
			s = concat(s, _to_string(get(v, i)))
			if i < n - 1 {
				s = concat(s, ", ")
			}
			i = i + 1
		}
		return concat(s, "]")
	}
	if is_dict(v) {
		return "{...}"
	}
	return rt_to_str(v)
}

fn _print_write(s) {
	"Internal: Writes a string to stdout without a newline."
	sys_write(1, s, str_len(s))
	return 0
}

fn print(vals, ...args) {
	def end = "\\n"
	def step = " "
	def n = list_len(args)
	def i = 0
	def cnt = 0
	while i < n {
		def item = get(args, i)
		if is_kwarg(item) {
			def k = get_kwarg_key(item)
			def v = get_kwarg_val(item)
			if _str_eq(k, "end") {
				end = v
			}
			if _str_eq(k, "step") || _str_eq(k, "sep") {
				step = v
			}
		} else {
			cnt = cnt + 1
		}
		i = i + 1
	}
	if is_kwarg(vals) == false {
		def s = _to_string(vals)
		sys_write(1, s, str_len(s))
		if cnt > 0 {
			sys_write(1, step, str_len(step))
		}
	}
	i = 0
	def printed = 0
	while i < n {
		def item = get(args, i)
		if is_kwarg(item) == false {
			def s = _to_string(item)
			sys_write(1, s, str_len(s))
			printed = printed + 1
			if printed < cnt {
				sys_write(1, step, str_len(step))
			}
		}
		i = i + 1
	}
	sys_write(1, end, str_len(end))
	return 0
}

fn input(prompt) {
	"Displays `prompt` and reads a line of input from stdin. Returns the input as a string."
	if prompt != 0 {
		sys_write(1, prompt, str_len(prompt))
	}
	def cap = IO_BUF
	def buf = rt_malloc(cap)
	store64(buf, 120, -8)
	def pos = 0
	while 1 {
		def n = sys_read(0, buf, cap - pos - 1)
		if n <= 0 {
			break
		}
		def i = 0
		while i < n {
			if load8(buf, pos + i) == 10 {
				pos = pos + i
				store8(buf, 0, pos)
				return buf
			}
			i = i + 1
		}
		pos = pos + n
		if pos + 1 >= cap {
			def newcap = cap * 2
			def new_buf = rt_malloc(newcap)
			store64(new_buf, 120, -8)
			memcpy(new_buf, buf, pos)
			rt_free(buf)
			buf = new_buf
			cap = newcap
		}
	}
	store8(buf, 0, pos)
	return buf
}

fn file_read(path) {
	"Reads the entire content of a file into a string."
	def fd = sys_open(path, 0, 0)
	if fd < 0 {
		return ""
	}
	def cap = IO_BUF
	def buf = rt_malloc(cap)
	store64(buf, 120, -8)
	def pos = 0
	while 1 {
		def n = sys_read(fd, buf, cap - pos - 1, pos)
		if n <= 0 {
			break
		}
		pos = pos + n
		if pos + 1 >= cap {
			def next_cap = cap * 2
			def next_buf = rt_malloc(next_cap)
			store64(next_buf, 120, -8)
			rt_memcpy(next_buf, buf, pos)
			rt_free(buf)
			buf = next_buf
			cap = next_cap
		}
	}
	sys_close(fd)
	store8(buf, 0, pos)
	return buf
}

fn file_write(path, data) {
	"Writes string `data` to the file at `path`, overwriting any existing content."
	def fd = sys_open(path, 577, 420)
	if fd < 0 {
		return -1
	}
	def n = sys_write(fd, data, str_len(data))
	sys_close(fd)
	return n
}

fn file_append(path, data) {
	"Appends string `data` to the end of the file at `path`."
	def fd = sys_open(path, 1089, 420)
	if fd < 0 {
		return -1
	}
	def n = sys_write(fd, data, str_len(data))
	sys_close(fd)
	return n
}

fn file_exists(path) {
	"Check if a file or directory exists at the specified path."
	def buf = rt_malloc(144)
	def r = sys_stat(path, buf)
	rt_free(buf)
	return r == 0
}

fn file_remove(path) {
	"Deletes the file at the specified path. System call."
	return rt_syscall(87, path, 0, 0, 0, 0, 0)
}

fn cwd() {
	"Return the current working directory as a string."
	def buf = rt_malloc(4096)
	rt_store64_idx(buf, -8, 120)
	def n = rt_syscall(79, buf, 4096, 0, 0, 0, 0)
	if n < 0 {
		rt_free(buf)
		return ""
	}
	rt_store8_idx(buf, n, 0)
	return buf
}

fn __repl_show(val) {
	"Internal: Used by REPL to display values."
	def s = rt_to_str(val)
	sys_write(1, s, str_len(s))
	sys_write(1, "\\n", 1)
	return val
}
