;;; test.ny --- core test module

;; Author: x3ric
;; Maintainer: x3ric
;; Keywords: core test

;;; Commentary:

;; Simple assertion-based testing framework.

use std.core.mod

def STDERR_FD = 2

fn write_stderr(s){
	"Writes the given string to stderr."
	rt_syscall(1, STDERR_FD, s, str_len(s))
}

fn t_assert(condition, message){
	"Asserts that a condition is true. If not, prints a message and exits."
	if(!condition){
		write_stderr("Assertion failed: ")
		write_stderr(message)
		write_stderr("\n")
		rt_exit(1)
	}
}

fn t_assert_eq(a, b, message){
	"Asserts that two values are equal."
	if(a != b){
		write_stderr("Equality assertion failed: ")
		write_stderr(message)
		write_stderr("\n")
		write_stderr("  Expected: ")
		write_stderr(b)
		write_stderr("\n")
		write_stderr("  Got: ")
		write_stderr(a)
		write_stderr("\n")
		rt_exit(1)
	}
}

fn fail(message){
	"Forces a test to fail."
	write_stderr("Test failed: ")
	write_stderr(message)
	write_stderr("\n")
	rt_exit(1)
}
