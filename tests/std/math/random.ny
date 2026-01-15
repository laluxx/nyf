use std.math.random
use std.core.error
fn test_rand() {
    print("Testing random...")
    seed(42)
    def r1 = rand()
    assert(r1 >= 0, "rand positive")
    def r2 = rand()
    assert(r1 != r2, "rand sequence varies")
    print("Rand values: ", r1, " ", r2)
    seed(42)
    def r3 = rand()
    print("Random basic passed")
}
fn test_randint() {
    print("Testing randint...")
    def i = 0
    while i < 100 {
        def v = randint(10, 20)
        assert(v >= 10, "randint min")
        assert(v < 20, "randint max")
        i = i + 1
    }
    print("Randint passed")
}
fn test_choice() {
    print("Testing choice...")
    def lst = list()
    append(lst, 10)
    append(lst, 20)
    append(lst, 30)
    def c = choice(lst)
    assert(c == 10 || c == 20 || c == 30, "choice validation")
    print("Choice passed")
}
fn test_main() {
    test_rand()
    test_randint()
    test_choice()
    print("✓ std.math.random passed")
}
test_main()