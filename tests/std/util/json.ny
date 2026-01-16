use std.core.mod
use std.core.reflect
use std.strings.str
use std.collections.mod
use std.collections.dict
;;; json.ny --- util json module
;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: util json
;;; Commentary:
;; JSON encode/decode helpers.
fn _json_hex2(n) {
	def h = "0123456789abcdef"
	return concat(slice(h, n>>4, n>>4+1), slice(h, n & 15, n & 15 + 1))
}

fn json_escape(s) {
	"Escapes a string for safe JSON emission."
	def out = list(8)
	def i = 0
	n = str_len(s)
	while i < n {
		def c = rt_load8_idx(s, i)
		if c == 34 {
			out = append(out, "\\\\"")
		} else {
			if c == 92 {
				out = append(out, "\\\\")
			} else {
				if c == 10 {
					out = append(out, "\\n")
				} else {
					if c == 9 {
						out = append(out, "\\t")
					} else {
						if c == 13 {
							out = append(out, "\\r")
						} else {
							if c < 32 {
								out = append(out, concat("\\u00", concat(_json_hex2(c), "")))
							} else {
								def ch = rt_malloc(2)
								rt_store8_idx(ch, 0, c)
								rt_store8_idx(ch, 1, 0)
								out = append(out, ch)
							}
						}
					}
				}
			}
		}
		i = i + 1
	}
	return join(out, "")
}

fn json_encode(v) {
	"Encodes a Nytrix value into JSON text."
	if is_list(v) == 1 || is_tuple(v) == 1 {
		def out = "["
		def i = 0
		n = list_len(v)
		while i < n {
			out = concat(out, json_encode(get(v, i)))
			if i+1 < n {
				out = concat(out, ",")
			}
			i = i+1
		}
		out = concat(out, "]")
		return out
	}
	if is_dict(v) == 1 {
		out = "{"
		i = 0
		n = list_len(v)
		while i < n {
			def p = load_item(v, i)
			out = concat(out, concat("\\"", concat(json_escape(get(p, 0)), "\\"")))
			out = concat(out, ":")
			out = concat(out, json_encode(get(p, 1)))
			if i+1 < n {
				out = concat(out, ",")
			}
			i = i+1
		}
		out = concat(out, "}")
		return out
	}
	if is_str(v) == 1 {
		return concat("\\"", concat(json_escape(v), "\\""))
	}
	return repr(v)
}

fn json_of(self) {
	"Function: json_of."
	return json_encode(self)
}
"JSON Decoding"
fn _json_skip_ws(s, i, n) {
	while i < n {
		def c = rt_load8_idx(s, i)
		if c != 32 && c != 10 && c != 13 && c != 9 {
			return i
		}
		i = i + 1
	}
	return i
}

fn _json_parse_val(s, i, n) {
	i = _json_skip_ws(s, i, n)
	if i >= n {
		return [0, i]
	}
	def c = rt_load8_idx(s, i)
	if c == 34 {
		return _json_parse_str(s, i, n)
	}
	if c == 91 {
		return _json_parse_list(s, i, n)
	}
	if c == 123 {
		return _json_parse_dict(s, i, n)
	}
	if c == 116 {
		return [true, i + 4]
	}
	"true"
	if c == 102 {
		return [false, i + 5]
	}
	"false"
	if c == 110 {
		return [0, i + 4]
	}
	"null"
	return _json_parse_num(s, i, n)
}

fn _json_parse_str(s, i, n) {
	i = i + 1
	"skip opening quote"
	def res = ""
	while i < n {
		def c = rt_load8_idx(s, i)
		if c == 34 {
			return [res, i + 1]
		}
		if c == 92 {
			"escape"
			i = i + 1
			def c2 = rt_load8_idx(s, i)
			if c2 == 110 {
				res = concat(res, "\\n")
			} else {
				if c2 == 116 {
					res = concat(res, "	")
				} else {
					if c2 == 114 {
						res = concat(res, "")
					} else {
						if c2 == 34 {
							res = concat(res, "\\"")
						} else {
							if c2 == 92 {
								res = concat(res, "\\")
							} else {
								def ch = rt_malloc(2)
								store64(ch - 8, 120)
								rt_store8_idx(ch, 0, c2)
								rt_store8_idx(ch, 1, 0)
								res = concat(res, ch)
							}
						}
					}
				}
			}
		} else {
			def ch = rt_malloc(2)
			store64(ch - 8, 120)
			rt_store8_idx(ch, 0, c)
			rt_store8_idx(ch, 1, 0)
			res = concat(res, ch)
		}
		i = i + 1
	}
	panic("json: unterminated string")
}

fn json_parse_num(s, i, n) {
	def start = i
	while i < n {
		def c = rt_load8_idx(s, i)
		if c < 48 || c > 57 {
			if i == start && c == 45 {
				i = i + 1
				continue
			}
			break
		}
		i = i + 1
	}
	def num_str = slice(s, start, i, 1)
	return [atoi(num_str), i]
}

fn _json_parse_list(s, i, n) {
	i = i + 1
	"skip ["
	def res = list(8)
	while i < n {
		i = _json_skip_ws(s, i, n)
		if rt_load8_idx(s, i) == 93 {
			return [res, i + 1]
		}
		def p = _json_parse_val(s, i, n)
		res = append(res, get(p, 0))
		i = get(p, 1)
		i = _json_skip_ws(s, i, n)
		if rt_load8_idx(s, i) == 44 {
			i = i + 1
		}
		"skip ,"
	}
	panic("json: unterminated list")
}

fn _json_parse_dict(s, i, n) {
	i = i + 1
	"skip brace"
	def res = dict(16)
	while i < n {
		i = _json_skip_ws(s, i, n)
		if rt_load8_idx(s, i) == 125 {
			return [res, i + 1]
		}
		def p_key = _json_parse_str(s, i, n)
		def key = get(p_key, 0)
		i = get(p_key, 1)
		i = _json_skip_ws(s, i, n)
		if rt_load8_idx(s, i) != 58 {
			panic("json: expected : in dict")
		}
		i = i + 1
		def p_val = _json_parse_val(s, i, n)
		setitem(res, key, get(p_val, 0))
		i = get(p_val, 1)
		i = _json_skip_ws(s, i, n)
		if rt_load8_idx(s, i) == 44 {
			i = i + 1
		}
		"skip ,"
	}
	panic("json: unterminated dict")
}

fn json_decode(s) {
	"Parse JSON string to value."
	def p = _json_parse_val(s, 0, str_len(s))
	return get(p, 0)
}
