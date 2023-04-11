import sys
import re
import operator
import functools

infile = "aoc11.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    raw_data = infilep.read().strip()
    data = re.findall("[a-z]+", raw_data)

def coord_to_dist(c):
    ns = int(c.real)
    ew = int(c.imag)
    if ns*ew > 0:
        return max(abs(ns), abs(ew))
    return abs(ns) + abs(ew)

coords = 0+0j
maxdist = 0
for step in data:
    coords += {'n': 1+0j, 's': -1+0j, 'nw': 0-1j, 'ne':1+1j, 'se': 0+1j, 'sw': -1-1j}[step]
    maxdist = max(maxdist, coord_to_dist(coords))

print(coord_to_dist(coords))
print(maxdist)
