(+ 3 3 3 3)
(func "a" 3)

fn test_multiple_captures() {
	print("Testing multiple captures...")
	def a = 1
	def b = 2
	def c = 3
	def f = lambda(){(+ a b c)}
	assert((f) == 6, "Capture a,b,c")
}

