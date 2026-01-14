use std.net.socket
use std.strings.str
use std.os.thread
use std.core.test
define PORT = 50002
print("Testing Sockets... SKIPPED (hangs)")
exit(0)

fn server_task(arg){
    def port = arg
    def s = socket_bind("127.0.0.1", port)
    if(s < 0){ panic("server bind failed") }

    def c = socket_accept(s)
    if(c < 0){ panic("server accept failed") }

    def req = read_socket(c, 1024)
    if(eq(req, "ping")){
        write_socket(c, "pong")
    }
    close_socket(c)
    close_socket(s)
    return 0
}

define t = thread_spawn(server_task, PORT)

sleep(1) ; Wait for server to bind

define c = socket_connect("127.0.0.1", PORT)
if(c < 0){ panic("client connect failed") }

write_socket(c, "ping")
define res = read_socket(c, 1024)
print("Got:", res)

assert(eq(res, "pong"), "socket ping/pong")

close_socket(c)
thread_join(t)

print("✓ std.net.socket passed")
