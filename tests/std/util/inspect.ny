use std
fn inspect(x) {
    "Prints detailed information about value x."
    def t = type(x)
    print(concat("Type:  ", t))
    print(concat("Value: ", repr(x)))
    if _str_eq(t, "list") || _str_eq(t, "dict") || _str_eq(t, "set") {
        print(concat("Len:   ", itoa(len(x))))
    }
    if is_ptr(x) {
        print(concat("Addr:  0x", itoa(x)))
    }
    return 0
}
