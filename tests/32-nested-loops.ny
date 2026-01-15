fn nested() {
    def i = 0
    while i < 5 {
        def j = 0
        while j < 3 {
            j += 1
        }
        i += 1
    }
}