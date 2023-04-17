import sys
import re


infile = "aoc9.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = infilep.read().strip()

output = ""
in_idx = 0
match_re = re.compile(r"\((\d+)x(\d+)\)|([^\(]+)|\s+")
while m := match_re.match(data, in_idx):
    in_idx = m.end(0)
    if m.group(3):
        output += m.group(3)
    elif m.group(1):
        nextbit = data[in_idx : in_idx + int(m.group(1))]
        in_idx += int(m.group(1))
        output += nextbit * int(m.group(2))

print(len(output))


def decompressed_size(val):
    outlen = 0
    in_idx = 0
    while m := match_re.match(val, in_idx):
        in_idx = m.end(0)
        if m.group(3):
            outlen += m.end(3) - m.start(3)
        elif m.group(1):
            nextbit = val[in_idx : in_idx + int(m.group(1))]
            in_idx += int(m.group(1))
            outlen += decompressed_size(nextbit) * int(m.group(2))
    return outlen


print(decompressed_size(data))
