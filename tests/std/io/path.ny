; Test std.io.path - Path manipulation
use std.io.path

; Path join
fn test_main(){
    ; Path join
    def p1 = path_join("/home", "user")
    assert(eq(p1, "/home/user"), "join two parts")
    def p2 = path_join("/home/user", "file.txt")
    assert(eq(p2, "/home/user/file.txt"), "join with file")
    def p3 = path_join("relative", "path")
    assert(eq(p3, "relative/path"), "join relative paths")

    ; Basename
    assert(eq(basename("/home/user/file.txt"), "file.txt"), "basename with path")
    assert(eq(basename("file.txt"), "file.txt"), "basename without path")
    def b = basename("/home/user/")
    print("DEBUG: basename('/home/user/') = '", b, "' len=", len(b))
    assert(eq(b, "user"), "basename of directory")

    ; Dirname
    assert(eq(dirname("/home/user/file.txt"), "/home/user"), "dirname with file")
    assert(eq(dirname("/home/user/"), "/home"), "dirname of directory")
    assert(eq(dirname("file.txt"), "."), "dirname of file without path")

    ; Normalize
    p1 = normalize("/home/user/../file.txt")
    assert(eq(p1, "/home/file.txt"), "normalize with ..")
    p2 = normalize("/home/./user/file.txt")
    assert(eq(p2, "/home/user/file.txt"), "normalize with .")
    p3 = normalize("/home//user///file.txt")
    assert(eq(p3, "/home/user/file.txt"), "normalize multiple slashes")
}
test_main()
print("✓ std.io.path tests passed")
