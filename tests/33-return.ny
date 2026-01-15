fn early_return(x) {
    ;; def a = [0..10]
    ;; def l = (0..10)
    ;; def t = (0,..10)

    ;; if x < 0 return 0
    ;; if x < 0 {
    ;;    return 0
    ;; }
    ;; if (x < 0) return 0

    if (x < 0) {
        return 0
    }

    return x * 2
}
