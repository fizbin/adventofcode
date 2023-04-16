import sys
import re

infile = "aoc1.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = infilep.read().strip()

pos = 0 + 0j
pos2 = None
dir = 1 + 0j
been_there = set()
for m in re.finditer(r"([LR])(\d+)", data):
    if m.group(1) == "L":
        dir *= 1j
    else:
        dir *= -1j
    for _ in range(int(m.group(2))):
        pos += dir
        if pos in been_there and pos2 is None:
            pos2 = pos
        been_there.add(pos)

print(int(abs(pos.real) + abs(pos.imag)))
print(int(abs(pos2.real) + abs(pos2.imag)))
