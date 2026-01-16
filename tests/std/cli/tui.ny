use std.strings.str
use std.core.reflect
use std.collections.mod
use std.os.time
use std.math.float
use std.core.mod
fn bold(s) {
	return concat("![1m", concat(s, "![0m"))
}

fn italic(s) {
	return concat("![3m", concat(s, "![0m"))
}

fn dim(s) {
	return concat("![2m", concat(s, "![0m"))
}

fn underline(s) {
	return concat("![4m", concat(s, "![0m"))
}

fn color(s, c) {
	def code = "37"
	if eq(c, "black") {
		code = "30"
	} else {
		if eq(c, "red") {
			code = "31"
		} else {
			if eq(c, "green") {
				code = "32"
			} else {
				if eq(c, "yellow") {
					code = "33"
				} else {
					if eq(c, "blue") {
						code = "34"
					} else {
						if eq(c, "magenta") {
							code = "35"
						} else {
							if eq(c, "cyan") {
								code = "36"
							} else {
								if eq(c, "white") {
									code = "37"
								} else {
									if eq(c, "gray") {
										code = "90"
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return concat("![", concat(code, concat("m", concat(s, "![0m"))))
}

fn style(text, color_name = "", is_bold = 0) {
	"Applies ANSI styling to text."
	def out = text
	if is_bold {
		out = concat("![1m", out)
	}
	if str_len(color_name) > 0 {
		out = color(out, color_name)
	}
	if is_bold || str_len(color_name) > 0 {
		out = concat(out, "![0m")
	}
	return out
}

fn panel(text, title = "", border_color = "white") {
	"Prints a styled panel with optional title."
	def l = str_len(text)
	def w = l + 4
	if str_len(title) > 0 {
		def tl = str_len(title)
		if tl + 4 > w {
			w = tl + 4
		}
	}
	"Top"
	def top = "╭"
	def i = 0
	while i < w - 2 {
		top = concat(top, "─")
		i = i + 1
	}
	top = concat(top, "╮")
	top = color(top, border_color)
	if str_len(title) > 0 {
		"Embed title in top border"
		"This is a bit tricky with simple string concat if we want to replace the chars"
		"easier to just print title above or doing a simple replacement if possible."
		"Actually, standard panels put title on the border."
		"We can reconstruct top."
		top = color("╭─ ", border_color)
		top = concat(top, color(title, "cyan"))
		top = concat(top, color(" ─", border_color))
		def rem = w - 2 - 2 - str_len(title) - 2
		i = 0
		while i < rem {
			top = concat(top, color("─", border_color))
			i = i + 1
		}
		top = concat(top, color("╮", border_color))
	}
	print(top)
	"Content"
	"Pad text to width"
	def padding = w - 4 - str_len(text)
	def line = concat("│ ", text)
	i = 0
	while i < padding {
		line = concat(line, " ")
		i = i + 1
	}
	line = concat(line, " │")
	"Color borders"
	def cbar = color("│", border_color)
	line = concat(cbar, concat(" ", concat(text, " ")))
	i = 0
	while i < padding {
		line = concat(line, " ")
		i = i + 1
	}
	line = concat(line, cbar)
	print(line)
	"Bottom"
	def bot = "╰"
	i = 0
	while i < w - 2 {
		bot = concat(bot, "─")
		i = i + 1
	}
	bot = concat(bot, "╯")
	print(color(bot, border_color))
}

fn table(headers, rows) {
	"Prints a simple table."
	"Calculate column widths"
	def cols = list_len(headers)
	def widths = list(8)
	def i = 0
	while i < cols {
		def w = str_len(get(headers, i))
		widths = append(widths, w)
		i = i + 1
	}
	def r = 0
	def nr = list_len(rows)
	while r < nr {
		def row = get(rows, r)
		def c = 0
		while c < cols {
			def val = get(row, c)
			def l = str_len(val)
			def cw = get(widths, c)
			if l > cw {
				set_idx(widths, c, l)
			}
			c = c + 1
		}
		r = r + 1
	}
	"Print headers"
	def line = ""
	i = 0
	while i < cols {
		def h = get(headers, i)
		w = get(widths, i)
		line = concat(line, bold(h))
		def pad = w - str_len(h) + 2
		def p = 0
		while p < pad {
			line = concat(line, " ")
			p = p + 1
		}
		line = concat(line, " ")
		i = i + 1
	}
	print(line)
	"Separator"
	def sep = ""
	i = 0
	while i < str_len(line) - 10 {
		"approximation minus ansi codes"
		sep = concat(sep, "─")
		i = i + 1
	}
	print(color(sep, "gray"))
	"Print rows"
	r = 0
	while r < nr {
		row = get(rows, r)
		line = ""
		c = 0
		while c < cols {
			val = get(row, c)
			w = get(widths, c)
			line = concat(line, val)
			pad = w - str_len(val) + 2
			p = 0
			while p < pad {
				line = concat(line, " ")
				p = p + 1
			}
			line = concat(line, " ")
			c = c + 1
		}
		print(line)
		r = r + 1
	}
}

fn tree(node, prefix = "") {
	"Prints a tree structure. Node is [label, [children...]] or just label string."
	if is_str(node) {
		print(concat(prefix, node))
		return 0
	}
	def label = get(node, 0)
	print(concat(prefix, bold(label)))
	def children = get(node, 1)
	def count = list_len(children)
	def i = 0
	while i < count {
		def last = i == count - 1
		def child = get(children, i)
		def conn = "├── "
		def next_prefix = concat(prefix, "│   ")
		if last {
			conn = "╰── "
			next_prefix = concat(prefix, "    ")
		}
		tree(child, concat(prefix, conn))
		i = i + 1
	}
}

fn bar(total = 100, desc = "Progress", width = 40, bar_color = "green", show_eta = 1, leave = 1) {
	"Create a progress bar. Returns a bar object (list)."
	def bar = list(8)
	bar = append(bar, total)
	"0: total"
	bar = append(bar, 0)
	"1: current"
	bar = append(bar, desc)
	"2: description"
	bar = append(bar, width)
	"3: width"
	bar = append(bar, bar_color)
	"4: bar_color"
	bar = append(bar, show_eta)
	"5: show_eta"
	bar = append(bar, leave)
	"6: leave"
	def start_time = ticks() / 1000000
	bar = append(bar, start_time)
	"7: start_time (ms)"
	bar = append(bar, start_time)
	"8: last_update_time (ms)"
	bar = append(bar, 0)
	"9: is_finished"
	bar = append(bar, 0)
	"10: last_update_val"
	bar = append(bar, 0.0)
	"11: avg_rate (items/sec)"
	return bar
}

fn bar_update(bar, current) {
	if get(bar, 9) == 1 {
		return 0
	}
	def last_val = get(bar, 10)
	if current <= last_val && current != 0 && current < get(bar, 0) {
		return 0
	}
	set_idx(bar, 1, current)
	set_idx(bar, 10, current)
	def total = get(bar, 0)
	def desc = get(bar, 2)
	def width = get(bar, 3)
	def bar_color = get(bar, 4)
	def show_eta = get(bar, 5)
	def start_time = get(bar, 7)
	def last_time = get(bar, 8)
	def now = ticks() / 1000000
	def dt = now - last_time
	def avg_rate = get(bar, 11)
	if dt > 150 || current == 1 || current == total {
		def elapsed_sec = now - start_time / 1000.0
		if elapsed_sec > 5.0e-2 {
			def inst_rate = current / elapsed_sec
			if avg_rate == 0.0 {
				avg_rate = inst_rate
			} else {
				avg_rate = avg_rate * 0.8 + inst_rate * 0.2
			}
			set_idx(bar, 11, avg_rate)
			set_idx(bar, 8, now)
		}
	}
	if total == 0 {
		total = 1
	}
	def pct = current * 100 / total
	if pct > 100 {
		pct = 100
	}
	def filled = current * width / total
	if filled > width {
		filled = width
	}
	def bar_str = repeat("█", filled)
	bar_str = color(bar_str, bar_color)
	def empty_str = repeat("░", width - filled)
	empty_str = color(empty_str, "gray")
	def rate_str = ""
	def eta_str = ""
	if show_eta && avg_rate > 1.0e-3 {
		rate_str = concat(" [", concat(itoa(int(avg_rate)), " it/s]"))
		def remaining = total - current
		if remaining > 0 {
			def eta_sec = 0
			if avg_rate > 1.0e-3 {
				eta_sec = int(float(remaining) / avg_rate)
			}
			if eta_sec < 60 {
				eta_str = concat(" ", concat(itoa(eta_sec), "s"))
			} else {
				eta_str = concat(" ", concat(itoa(eta_sec / 60), concat("m", concat(itoa(eta_sec % 60), "s"))))
			}
		}
	}
	"Format: ![KDesc: 100%|████| 10/10 [12it/s 10s]"
	def pct_s = itoa(pct)
	if pct < 10 {
		pct_s = concat("  ", pct_s)
	} else {
		if pct < 100 {
			pct_s = concat(" ", pct_s)
		}
	}
	def out = "![K"
	if str_len(desc) > 0 {
		out = concat(out, concat(desc, ": "))
	}
	out = concat(out, concat(pct_s, "%|"))
	out = concat(out, concat(bar_str, concat(empty_str, "| ")))
	out = concat(out, concat(itoa(current), concat("/", concat(itoa(total), ""))))
	out = concat(out, concat(rate_str, eta_str))
	sys_write(1, out, str_len(out))
	if current >= total {
		set_idx(bar, 9, 1)
	}
	return 0
}

fn bar_finish(bar) {
	def total = get(bar, 0)
	def current = get(bar, 1)
	if current < total {
		bar_update(bar, total)
	}
	def leave = get(bar, 6)
	if leave {
		print("")
	} else {
		sys_write(1, "", 1)
		def clear_line = "![K"
		sys_write(1, clear_line, str_len(clear_line))
	}
	set_idx(bar, 9, 1)
	return 0
}

fn bar_range(n, desc = "") {
	return bar(n, desc)
}

fn bar_write(bar_obj, msg) {
	sys_write(1, "", 1)
	def clear_line = "![K"
	sys_write(1, clear_line, str_len(clear_line))
	print(msg)
	bar_update(bar_obj, get(bar_obj, 1))
	return 0
}
