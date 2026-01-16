use core.core
fn thread_spawn(func, arg = 0) {
    "Spawns a new thread executing `func(arg)`. Returns the thread handle."
    return rt_thread_spawn(func, arg)
}

fn thread_join(handle) {
    "Waits for the thread `handle` to finish and returns its result."
    return rt_thread_join(handle)
}

fn sleep(ms) {
    "Sleeps for `ms` milliseconds."
    return rt_sleep(ms)
}

fn mutex_new() {
    "Create a new mutex."
    return rt_mutex_new()
}

fn mutex_lock(m) {
    "Acquires the mutex `m`. Blocks if unavailable."
    return rt_mutex_lock64(m)
}

fn mutex_unlock(m) {
    "Releases the mutex `m`."
    return rt_mutex_unlock64(m)
}

fn mutex_free(m) {
    "Destroys the mutex `m`."
    return rt_mutex_free(m)
}
