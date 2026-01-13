fn sum(...args) {
    def total = 0
    def i = 0
    while(i < list_len(args)) {
        total += get(args, i)
        i += 1
    }
    return total
}
