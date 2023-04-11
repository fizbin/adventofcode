import sys
import re

infile = "aoc9.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = infilep.read()

data = data.strip(" \r\n\t")
garbage_re = re.compile(r"<((?:[^!>]|!.)*)>")
garbage_re2 = re.compile(r"([^!>]+)|(?:!.)+")
idx = 0
level = 1
mysum = 0
garbage_count = 0
while idx < len(data):
    ch = data[idx]
    if ch == "{":
        mysum += level
        level += 1
    elif ch == "}":
        level -= 1
    elif ch == "<":
        m = garbage_re.match(data, idx)
        idx = m.end(0)
        for m in garbage_re2.finditer(m.group(1)):
            garbage_count += m.end(1) - m.start(1)
        continue
    idx += 1
print(mysum)
print(garbage_count)
