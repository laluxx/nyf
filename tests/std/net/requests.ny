use net.http
fn requests_get(url) {
    "High-level helper to perform an HTTP GET request using a URL. Returns the response body."
    return http_get_url(url)
}

fn requests_get_host(host, port, path) {
    "Helper to perform an HTTP GET request using host, port, and path."
    return http_get(host, port, path)
}
