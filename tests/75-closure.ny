fn make_counter() {
    def count = 0
    return lambda() {
        count += 1
        return count
    }
}