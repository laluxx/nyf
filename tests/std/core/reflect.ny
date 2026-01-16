use std.core.mod
use std.core.error
use std.strings.str
use std.collections.mod
;; reflect.ny --- High-level reflection and polymorphism
;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: core reflect
;;; Commentary:
;; Provides high-level reflection, polymorphic operations, and value inspection.
fn len(x) {
	"Return the number of items in a collection or the length of a string."
	if x == 0 {
		return 0
	}
	if is_list(x) {
		return list_len(x)
	}
	if is_tuple(x) {
		return list_len(x)
	}
	if is_dict(x) {
		return load64(x)
	}
	if is_set(x) {
		return list_len(x)
	}
	if is_ptr(x) {
		return str_len(x)
	}
	return 0
}

fn contains(container, item) {
	"Check if an item exists within a collection (list, dict keys, set, or as a substring in a string)."
	if !container {
		return false
	}
	if is_set(container) {
		return set_contains(container, item)
	}
	if is_dict(container) {
		return has(container, item)
	}
	if is_list(container) {
		return list_contains(container, item)
	}
	if is_str(container) {
		return find(container, item) >= 0
	}
	return false
}

fn type(x) {
	if x == 0 {
		return "none"
	}
	if rt_is_int(x) {
		return "int"
	}
	if is_ptr(x) {
		if is_list(x) {
			return "list"
		}
		if is_dict(x) {
			return "dict"
		}
		if is_set(x) {
			return "set"
		}
		if is_tuple(x) {
			return "tuple"
		}
		if is_str(x) {
			return "str"
		}
		return "ptr"
	}
	if x == true || x == false {
		return "bool"
	}
	return "unknown"
}

fn list_eq(a, b) {
	"Deep equality comparison for lists."
	if list_len(a) != list_len(b) {
		return false
	}
	def i = 0
	def n = list_len(a)
	while i < n {
		if eq(get(a, i), get(b, i)) == false {
			return false
		}
		i = i + 1
	}
	return true
}

fn dict_eq(a, b) {
	"Deep equality comparison for dictionaries."
	if len(a) != len(b) {
		return false
	}
	def its = items(a)
	def i = 0
	def n = list_len(its)
	while i < n {
		def p = get(its, i)
		if eq(getitem(b, p[0], 3735928559), p[1]) == false {
			return false
		}
		i = i+1
	}
	return true
}

fn set_eq(a, b) {
	"Deep equality comparison for sets."
	if len(a) != len(b) {
		return false
	}
	def its = items(a)
	def i = 0
	def n = list_len(its)
	while i < n {
		def p = get(its, i)
		if set_contains(b, p[0]) == false {
			return false
		}
		i = i+1
	}
	return true
}

fn eq(a, b) {
	"Structural equality check. Compares values by content for strings and collections, and by value for integers."
	if a == b {
		return true
	}
	if is_ptr(a) == false {
		return false
	}
	if is_ptr(b) == false {
		return false
	}
	def ta = type(a)
	def tb = type(b)
	if !_str_eq(ta, tb) {
		return false
	}
	if _str_eq(ta, "list") {
		return list_eq(a, b)
	}
	if _str_eq(ta, "dict") {
		return dict_eq(a, b)
	}
	if _str_eq(ta, "set") {
		return set_eq(a, b)
	}
	return _str_eq(a, b)
}

fn str(x) {
	"Return a human-readable string representation of the value.
For strings, returns the string itself."
	if is_ptr(x) {
		if is_str(x) {
			return x
		}
	}
	return repr(x)
}

fn repr(x) {
	if x == 0 {
		return "none"
	}
	if x == true {
		return "true"
	}
	if x == false {
		return "false"
	}
	def t = type(x)
	if eq(t, "list") {
		def n = list_len(x)
		def out = "["
		def i = 0
		while i < n {
			out = concat(out, repr(get(x, i)))
			if i+1 < n {
				out = concat(out, ",")
			}
			i = i+1
		}
		return concat(out, "]")
	}
	if eq(t, "dict") {
		def its = items(x)
		def out = "{"
		def i = 0
		def n = list_len(its)
		while i < n {
			def p = get(its, i)
			out = concat(out, concat(repr(p[0]), concat(":", repr(p[1]))))
			if i+1 < n {
				out = concat(out, ",")
			}
			i = i+1
		}
		return concat(out, "}")
	}
	if eq(t, "set") {
		def its = items(x)
		def out = "{"
		def i = 0
		def n = list_len(its)
		while i < n {
			def p = get(its, i)
			out = concat(out, repr(p[0]))
			if i+1 < n {
				out = concat(out, ",")
			}
			i = i+1
		}
		return concat(out, "}")
	}
	if eq(t, "str") {
		return concat("\\"", concat(x, "\\""))
	}
	if eq(t, "int") {
		return itoa(x)
	}
	if eq(t, "ptr") {
		return concat("<ptr ", concat(itoa(x), ">"))
	}
	return itoa(x)
}

fn hash(x) {
	"Return a 64-bit FNV-1a hash of value x."
	def t = type(x)
	if _str_eq(t, "int") {
		return x
	}
	if _str_eq(t, "str") {
		def h = 14695981039346656037
		def i = 0
		def n = str_len(x)
		while i < n {
			h = h ^ load8(x + i) * 1099511628211
			i = i + 1
		}
		return h
	}
	return x
}

fn globals() {
	"Return the current global symbol table as a dictionary."
	return rt_globals()
}
