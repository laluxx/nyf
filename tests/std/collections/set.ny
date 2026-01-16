
;;; set.ny --- collections set module

;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: collections set

;;; Commentary:

;; Set implementation backed by List-Map

use std.core.mod
use std.core.reflect
use std.collections.dict ; for dict(16), setitem(), has(), delitem(), dict_clear(), items()

def SET_MAGIC = 102

fn set(){
	"Create a new empty set."
	def d = dict(16)
	store64(d, SET_MAGIC, -8)
	return d
}

fn add(s, v){
	"Adds an element to the set."
	return setitem(s, v, 1)
}

fn set_contains(s, v){
	"Check if value v is in the set."
	if(has(s, v)){ return true }
	return false
}

fn remove(s, v){
	"Removes value v from the set."
	return delitem(s, v)
}

fn set_clear(s){
	"Removes all elements from the set."
	return dict_clear(s)
}

fn set_copy(s){
	"Return a shallow copy of the set."
	def out = set()
	def its = items(s)
	def i = 0 def n = list_len(its)
	while(i < n){
		def p = get(its, i)
		out = add(out, p[0])
		i = i + 1
	}
	return out
}

fn set_union(a, b){
	"Return the union of two sets."
	def out = set_copy(a)
	def its = items(b)
	def i = 0 def n = list_len(its)
	while(i < n){
		def p = get(its, i)
		out = add(out, p[0])
		i = i + 1
	}
	return out
}

fn set_intersection(a, b){
	"Return the intersection of two sets."
	def out = set()
	def its = items(a)
	def i = 0 def n = list_len(its)
	while(i < n){
		def p = get(its, i)
		def v = p[0]
		if(set_contains(b, v)){
			out = add(out, v)
		}
		i = i + 1
	}
	return out
}

fn set_difference(a, b){
	"Return the difference of two sets (a - b)."
	def out = set()
	def its = items(a)
	def i = 0 def n = list_len(its)
	while(i < n){
		def p = get(its, i)
		def v = p[0]
		if(set_contains(b, v) == false){
			out = add(out, v)
		}
		i = i + 1
	}
	return out
}

fn is_set(s){
	if(!rt_is_ptr(s)){ return 0 }
	return load64(s, -8) == SET_MAGIC
}
