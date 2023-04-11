import sys
import re
import collections

infile = "aoc13.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

dl_list = []
for line in data:
    m = re.match(r"(\d+) *: *(\d+)", line)
    dl_list.append((int(m.group(1)), int(m.group(2))))

dl_list.sort()
cycle_list = [(a, b, 1 if b == 1 else 2 * b - 2) for (a, b) in dl_list]
badness = 0
for depth, range, cycle_size in cycle_list:
    if depth % cycle_size == 0:
        badness += depth * range
print(badness)


class GotCaught(Exception):
    pass


delay = 0
while True:
    delay += 1
    try:
        for depth, range, cycle_size in cycle_list:
            if (delay + depth) % cycle_size == 0:
                raise GotCaught()
    except GotCaught:
        continue
    print(delay)
    break
