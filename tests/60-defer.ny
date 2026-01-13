fn with_cleanup() {
    def res = allocate()
    defer {
        cleanup(res)
    }
    return process(res)
}
