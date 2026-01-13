fn greet(first, last, title="Mr") {
    return f"{title} {first} {last}"
}

define result = greet(first="John", last="Doe")
define result2 = greet("Jane", last="Smith", title="Dr")
