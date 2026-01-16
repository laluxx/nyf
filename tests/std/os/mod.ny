use std.core.mod
use std.strings.str
use std.collections.mod
fn pid() {
    "Process id."
    return rt_syscall(39, 0, 0, 0, 0, 0, 0)
}

fn os_pid() {
    "Method-style alias."
    return pid()
}

fn ppid() {
    "Parent pid."
    return rt_syscall(110, 0, 0, 0, 0, 0, 0)
}

fn os_ppid() {
    "Method-style alias."
    return ppid()
}

fn _cstr_to_str(p) {
    if !p {
        return ""
    }
    def n = 0
    while load8(p, n) != 0 {
        n = n + 1
    }
    def out = rt_malloc(n + 1)
    store64(out - 8, 120)
    "Tag Str"
    def i = 0
    while i < n {
        store8(out, load8(p, i), i)
        i = i + 1
    }
    store8(out, 0, n)
    return out
}

fn env(name) {
    "Get environment variable."
    def envp = rt_envp()
    if !envp {
        return 0
    }
    def i = 0
    while load64(envp, i*8) {
        def s_raw = load64(envp, i*8)
        def s = _cstr_to_str(s_raw)
        if startswith(s, name) && load8(s_raw + str_len(name)) == 61 {
            return slice(s, str_len(name) + 1, str_len(s), 1)
        }
        i = i + 1
    }
    return 0
}

fn environ() {
    "All environment variables as a list of strings."
    def envp = rt_envp()
    if !envp {
        return list(8)
    }
    def xs = list(8)
    def i = 0
    while load64(envp, i*8) {
        def s_raw = load64(envp, i*8)
        xs = append(xs, _cstr_to_str(s_raw))
        i = i + 1
    }
    return xs
}

fn getcwd() {
    "Current working directory."
    return cwd()
}

fn uid() {
    "Get uid/gid."
    return rt_syscall(102, 0, 0, 0, 0, 0, 0)
}

fn gid() {
    "Function: gid."
    return rt_syscall(104, 0, 0, 0, 0, 0, 0)
}

fn os_uid() {
    "Function: os_uid."
    return uid()
}

fn os_gid() {
    "Function: os_gid."
    return gid()
}
