fn find_first() {
    def i = 0
    while(i < 100) {
        if(i == 42) {
            break
        }
        i += 1
    }
    return i
}
