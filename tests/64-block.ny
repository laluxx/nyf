fn scoped() {
    def x = 10
    {
        def y = 20
        x = x + y
    }

    print(y); <- ERROR Use of undeclared symbol 'y' [undeclared_var_use]
    return x
}
