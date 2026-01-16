use std.core.mod
use std.net.http
use std.util.url
use std.collections.mod
fn request(method, url, data = 0) {
    "Performs an HTTP request."
    def parts = parse_url(url)
    def host = get(parts, 0)
    def port = get(parts, 1)
    def path = get(parts, 2)
    if eq(method, "GET") {
        return http_get(host, port, path)
    }
    if eq(method, "POST") {
        return http_post(host, port, path, data)
    }
    if eq(method, "PUT") {
        return http_put(host, port, path, data)
    }
    if eq(method, "DELETE") {
        return http_delete(host, port, path)
    }
    return 0
}

fn urlopen(url) {
    "Opens a URL and returns the response body."
    return request("GET", url, 0)
}
