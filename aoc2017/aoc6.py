import sys
import re

infile = "aoc6.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = [int(x) for x in re.findall(r"-?\d+", infilep.read())]

seen = dict()
steps = 0
while tuple(data) not in seen:
    seen[tuple(data)] = steps
    idx = data.index(max(data))
    blocks = data[idx]
    data[idx] = 0
    while blocks:
        idx += 1
        data[idx % len(data)] += 1
        blocks -= 1
    steps += 1
print(steps)
print(steps - seen[tuple(data)])
