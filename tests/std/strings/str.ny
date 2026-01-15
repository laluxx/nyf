; Test std.strings.str - String operations
use std.strings.str
use std.core.core

; Multiline Comments

"""
str/comment
"""

'''
str/comment
'''


"""
raw str/comment
"""
'''
raw str/comment
'''

; Single Line Comments

"str/comment"

'str/comment'


"raw str/comment"

'raw str/comment'

; String Operations

fn test_main(){
    ; String basics
    def str_val = "hello"
    assert(str_len(str_val) == 5, "string length")
    assert(eq(str_val, "hello"), "string equality")

    ; Concat
    def s1 = "hello"
    def s2 = " world"
    def s3 = concat(s1, s2)
    assert(eq(s3, "hello world"), "concatenation")

    ; Multiple concat
    def s4 = concat(concat("a", "b"), "c")
    assert(eq(s4, "abc"), "multiple concat")

    ; Slice
    str_val = "hello world"
    assert(eq(slice(str_val, 0, 5, 1), "hello"), "slice start to middle")
    print("DEBUG: str_val before second slice: '", str_val, "' len=", str_len(str_val))
    assert(eq(slice(str_val, 6, 11, 1), "world"), "slice middle to end")
    assert(eq(slice(str_val, 0, 11, 2), "hlowrd"), "slice with step")

    ; Contains
    str_val = "hello world"
    assert(str_contains(str_val, "world"), "contains substring")
    assert(str_contains(str_val, "hello"), "contains at start")
    assert(!str_contains(str_val, "xyz"), "doesn't contain")

    ; Split join
    str_val = "apple,banana,cherry"
    def parts = split(str_val, ",")
    assert(len(parts) == 3, "split length")
    assert(eq(get(parts, 0), "apple"), "split first")
    assert(eq(get(parts, 1), "banana"), "split second")
    assert(eq(get(parts, 2), "cherry"), "split third")

    ; Join back
    def joined = join(parts, ",")
    assert(eq(joined, str_val), "join restores original")

    ; Join with different separator
    def joined2 = join(parts, "-")
    assert(eq(joined2, "apple-banana-cherry"), "join with dash")

    ; Replace
    str_val = "hello world"
    def s2 = replace(str_val, "world", "Nytrix")
    assert(eq(s2, "hello Nytrix"), "replace substring")
    def s3 = replace("aaa", "a", "b")
    assert(eq(s3, "bbb"), "replace all occurrences")

    ; Case conversion
    str_val = "Hello World"
    def lower = to_lower(str_val)
    def upper = to_upper(str_val)
    print("DEBUG: lower: '", lower, "'")
    print("DEBUG: upper: '", upper, "'")
    assert(eq(lower, "hello world"), "to_lower")
    assert(eq(upper, "HELLO WORLD"), "to_upper")

    ; Trim
    str_val = "  hello  "
    def trimmed = trim(str_val)
    assert(eq(trimmed, "hello"), "trim whitespace")
    def trimmed2 = trim("hello")
    assert(eq(trimmed2, "hello"), "trim no whitespace")

    ; StartsWith EndsWith
    str_val = "hello world"
    assert(startswith(str_val, "hello"), "starts with")
    assert(!startswith(str_val, "world"), "doesn't start with")
    assert(endswith(str_val, "world"), "ends with")
    assert(!endswith(str_val, "hello"), "doesn't end with")
}

test_main()
print("✓ std.strings.str tests passed")
