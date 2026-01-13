fn safe_op() {
    try {
        return risky_operation()
    } catch {
        return default_value()
    }
}
