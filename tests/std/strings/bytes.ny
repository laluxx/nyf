;;; bytes.ny --- strings bytes module

;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: strings bytes

;;; Commentary:

;; Byte buffer helpers (create/put/load).

fn bytes(n){
	"Create zeroed byte buffer."
	def p = rt_malloc(n + 8) ; 8 bytes for length, n for data
	store64(p - 8, n) ; Store length at p-8
	memset(p, 0, n)
	return p
}
fn bytes_from_str(s){
	"Copy string to bytes buffer."
	def n = str_len(s)
	def buf = rt_malloc(n + 8)
	rt_store64_idx(buf, -8, n) ; Store length at p-8
	memcpy(buf, s, n)
	return buf
}

fn bytes_len(b){
	"Return length of bytes."
	if(b==0){ return 0  }
	return rt_load64_idx(b, -8)
}

fn bget(b, i){
	"Get byte."
	if(i<0 || i>=bytes_len(b)){ return 0  }
	return rt_load8_idx(b, i)
}

fn bset(b, i, v){
	"Set byte."
	rt_store8_idx(b, i, v)
	return b
}
;; ...

fn beq(a,b){
	"Bytes equality."
	def la=bytes_len(a)  def lb=bytes_len(b)
	if(la!=lb){ return false  }
	def i=0
	while(i<la){
		if(rt_load8_idx(a, i)!=rt_load8_idx(b, i)){ return false  }
		i=i+1
	}
	return true
}

fn bslice(b, start, stop){
	"Slice bytes [start, stop)."
	def n = bytes_len(b)
	if(start<0){ start=0  }
	if(stop<0 || stop>n){ stop=n  }
	if(stop<start){ stop=start  }
	def len = stop - start
	def out = rt_malloc(len + 8)
	rt_store64_idx(out, -8, len)
	memcpy(out, b + start, len)
	return out
}

fn bconcat(a,b){
	"Concat bytes."
	def la=bytes_len(a)  def lb=bytes_len(b)
	def out = rt_malloc(la+lb + 8)
	rt_store64_idx(out, -8, la + lb)
	memcpy(out, a, la)
	memcpy(out + la, b, lb)
	return out
}

fn bytes_to_str(b){
	"Convert bytes to string."
	def n = bytes_len(b)
	def s = rt_malloc(n + 1)
	rt_store64_idx(s, -8, 120) ; TAG_STR
	memcpy(s, b, n)
	store8(s + n, 0)
	return s
}

fn hex_encode(b){
	"Hex encode bytes."
	def hex = "0123456789abcdef"
	def n = bytes_len(b)
	def out = rt_malloc(n*2 + 8)
	rt_store64_idx(out, -8, n*2)
	def i=0  def o=0
	while(i<n){
		def v = rt_load8_idx(b, i)
		rt_store8_idx(out, o, rt_load8_idx(hex, ((v >> 4) & 15)))  o=o+1
		rt_store8_idx(out, o, rt_load8_idx(hex, (v & 15)))  o=o+1
		i=i+1
	}
	return out
}





fn _hex_val(c){
	"Function: _hex_val."
	if(c>=48 && c<=57){ return c-48  }
	if(c>=97 && c<=102){ return 10 + (c-97)  }
	if(c>=65 && c<=70){ return 10 + (c-65)  }
	return -1
}
fn hex_decode(s){
	"Hex decode string to bytes (ignores invalid, stops on odd length)."
	def n = str_len(s)
	def len_out = n/2
	def out = rt_malloc(len_out + 8)
	rt_store64_idx(out, -8, len_out)
	def i=0  def o=0
	while(i+1<n){
		def a = _hex_val(rt_load8_idx(s, i))
		def b = _hex_val(rt_load8_idx(s, i+1))
		if(a<0 || b<0){ break  }
		rt_store8_idx(out, o, (a<<4) + b)
		o=o+1  i=i+2
	}
	return out
}
