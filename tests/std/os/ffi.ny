;;; ffi.ny --- Foreign Function Interface

;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: os ffi

;;; Commentary:

;; Provides low-level FFI bindings to calls.

use std.core.mod

fn RTLD_LAZY(){
	"FFI: dlopen/dlsym wrappers and generic call shims."
	return 1  }
fn RTLD_NOW(){
	"Function: RTLD_NOW."
	return 2  }
fn RTLD_GLOBAL(){
	"Function: RTLD_GLOBAL."
	return 256  }
fn RTLD_LOCAL(){
	"Function: RTLD_LOCAL."
	return 0  }

fn dlopen(path, flags){
	"Open shared object, returns handle or 0."
	return rt_dlopen(path, flags)  }
fn dlsym(handle, name){
	"Lookup symbol, returns function/data pointer as int."
	return rt_dlsym(handle, name)  }
fn dlclose(handle){
	"Close handle."
	return rt_dlclose(handle)  }
fn dlerror(){
	"Last dl error string (char*)."
	return rt_dlerror()  }

fn call0_int(fptr){ return rt_call_int0(fptr) }
fn call1_int(fptr,a){ return rt_call_int1(fptr,a) }
fn call2_int(fptr,a,b){ return rt_call_int2(fptr,a,b) }
fn call3_int(fptr,a,b,c){ return rt_call_int3(fptr,a,b,c) }

fn call0_void(fptr){ rt_call0(fptr)  return 0 }
fn call1_void(fptr,a){ rt_call1(fptr,a)  return 0 }
fn call2_void(fptr,a,b){ rt_call2(fptr,a,b)  return 0 }
fn call3_void(fptr,a,b,c){ rt_call3(fptr,a,b,c)  return 0 }

fn call0(fptr){
	"Call fnptr with 0-3 int64 args."
	return rt_call0(fptr)  }
fn call1(fptr,a){
	"Function: call1."
	return rt_call1(fptr,a)  }
fn call2(fptr,a,b){
	"Function: call2."
	return rt_call2(fptr,a,b)  }
fn call3(fptr,a,b,c){
	"Function: call3."
	return rt_call3(fptr,a,b,c)  }
fn call4(fptr,a,b,c,d){
	"Function: call4."
	return rt_call4(fptr,a,b,c,d)  }
fn call5(fptr,a,b,c,d,e){
	"Function: call5."
	return rt_call5(fptr,a,b,c,d,e)  }
fn call6(fptr,a,b,c,d,e,g){
	"Function: call6."
	return rt_call6(fptr,a,b,c,d,e,g)  }
fn call7(fptr,a,b,c,d,e,g,h){
	"Function: call7."
	return rt_call7(fptr,a,b,c,d,e,g,h)  }
fn call8(fptr,a,b,c,d,e,g,h,i){
	"Function: call8."
	return rt_call8(fptr,a,b,c,d,e,g,h,i)  }
fn call9(fptr,a,b,c,d,e,g,h,i,j){
	"Function: call9."
	return rt_call9(fptr,a,b,c,d,e,g,h,i,j)  }
fn call10(fptr,a,b,c,d,e,g,h,i,j,k){
	"Function: call10."
	return rt_call10(fptr,a,b,c,d,e,g,h,i,j,k)  }
fn call11(fptr,a,b,c,d,e,g,h,i,j,k,l){
	"Function: call11."
	return rt_call11(fptr,a,b,c,d,e,g,h,i,j,k,l)  }
fn call12(fptr,a,b,c,d,e,g,h,i,j,k,l,m){
	"Function: call12."
	return rt_call12(fptr,a,b,c,d,e,g,h,i,j,k,l,m)  }
fn call13(fptr,a,b,c,d,e,g,h,i,j,k,l,m,n){
	"Function: call13."
	return rt_call13(fptr,a,b,c,d,e,g,h,i,j,k,l,m,n)  }

fn ffi_open(path, flags=2){
	"Convenience aliases."
	return dlopen(path, flags)
}

fn ffi_sym(handle, name){
	"Function: ffi_sym."
	return dlsym(handle, name)
}

fn ffi_close(handle){
	"Function: ffi_close."
	return dlclose(handle)
}

fn ffi_callfptr, args){
	"Call with list args (0-12 supported)."
	def n = list_len(args)
	if(n==0){ return call0(fptr)  }
	if(n==1){ return call1(fptr, get(args,0))  }
	if(n==2){ return call2(fptr, get(args,0), get(args,1))  }
	if(n==3){ return call3(fptr, get(args,0), get(args,1), get(args,2))  }
	if(n==4){ return call4(fptr, get(args,0), get(args,1), get(args,2), get(args,3))  }
	if(n==5){ return call5(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4))  }
	if(n==6){ return call6(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4), get(args,5))  }
	if(n==7){ return call7(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4), get(args,5), get(args,6))  }
	if(n==8){ return call8(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4), get(args,5), get(args,6), get(args,7))  }
	if(n==9){ return call9(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4), get(args,5), get(args,6), get(args,7), get(args,8))  }
	if(n==10){ return call10(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4), get(args,5), get(args,6), get(args,7), get(args,8), get(args,9))  }
	if(n==11){ return call11(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4), get(args,5), get(args,6), get(args,7), get(args,8), get(args,9), get(args,10))  }
	if(n==12){ return call12(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4), get(args,5), get(args,6), get(args,7), get(args,8), get(args,9), get(args,10), get(args,11))  }
	if(n==13){ return call13(fptr, get(args,0), get(args,1), get(args,2), get(args,3), get(args,4), get(args,5), get(args,6), get(args,7), get(args,8), get(args,9), get(args,10), get(args,11), get(args,12))  }
	panic("ffi_call supports 0-12 args")
	return 0
}
