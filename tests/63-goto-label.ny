fn jump_test() {
    def x = 0
start:
    x += 1
    if(x < 10) {
        goto start
    }
    return x
}
