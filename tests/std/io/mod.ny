use std.core.mod
fn write_fd(fd, buf, len) {
    return sys_write(fd, buf, len)
}

fn read_fd(fd, buf, n) {
    return sys_read(fd, buf, n)
}

fn io_print(self) {
    "Method-style aliases."
    return print(self)
}
