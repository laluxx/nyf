use std.core.mod
use std.math.mod
use std.strings.str
fn _is_leap(y) {
    "Time formatting (UTC)."
    if y % 4 != 0 {
        return 0
    }
    if y % 100 != 0 {
        return 1
    }
    if y % 400 != 0 {
        return 0
    }
    return 1
}

fn _days_in_month(y, m) {
    "Function: _days_in_month."
    if m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 {
        return 31
    }
    if m == 4 || m == 6 || m == 9 || m == 11 {
        return 30
    }
    if m == 2 {
        return 28 + _is_leap(y)
    }
    return 30
}

fn _days_in_year(y) {
    "Function: _days_in_year."
    return 365 + _is_leap(y)
}

fn _pad2(n) {
    "Function: _pad2."
    return pad_start(itoa(n), 2, "0")
}

fn _pad4(n) {
    "Function: _pad4."
    return pad_start(itoa(n), 4, "0")
}

fn format_time(ts) {
    "Format unix seconds to YYYY-MM-DD HH:MM:SS (UTC)."
    if ts < 0 {
        ts = 0
    }
    def days = ts / 86400
    def rem = ts - days*86400
    def hour = rem / 3600
    rem = rem - hour*3600
    def minute = rem / 60
    def second = rem - minute*60
    def year = 1970
    while days >= _days_in_year(year) {
        days = days - _days_in_year(year)
        year = year + 1
    }
    def month = 1
    while days >= _days_in_month(year, month) {
        days = days - _days_in_month(year, month)
        month = month + 1
    }
    def day = days + 1
    return concat(concat(concat(concat(_pad4(year), "-"), _pad2(month)), "-"), concat(concat(_pad2(day), " "), concat(concat(_pad2(hour), ":"), concat(concat(_pad2(minute), ":"), _pad2(second)))))
}
