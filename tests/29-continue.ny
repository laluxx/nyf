fn skip_evens() {
    def i = 0
    def count = 0
    while(i < 10) {
        i += 1
        if(i % 2 == 0) {
            continue
        }
        count += 1
    }
    return count
}
