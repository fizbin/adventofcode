#!/usr/bin/env python

import cmath
from cmath import phase
from aoc_util import get_data, chargrid

data = get_data(10)
grid = chargrid(data)

# | is a vertical pipe connecting north and south.
# - is a horizontal pipe connecting east and west.
# L is a 90-degree bend connecting north and east.
# J is a 90-degree bend connecting north and west.
# 7 is a 90-degree bend connecting south and west.
# F is a 90-degree bend connecting south and east.
# . is ground; there is no pipe in this tile.

pipedir = {
    "|": (-1, 1),
    "-": (-1j, 1j),
    "L": (-1, 1j),
    "J": (-1, -1j),
    "7": (1, -1j),
    "F": (1, 1j),
    ".": (),
    "S": (1, -1, 1j, -1j),
}

spot = None
gridd = {}
for rowidx, row in enumerate(grid):
    for colidx, ch in enumerate(row):
        gridd[rowidx + colidx * 1j] = ch
        if ch == "S":
            spot = rowidx + colidx * 1j

d_from_s = {spot: 0}
working = set([spot])
while working:
    new_working = set()
    for w in working:
        for d in pipedir[gridd[w]]:
            if d + w in gridd and -d in pipedir[gridd[d + w]]:
                if d + w not in d_from_s:
                    d_from_s[d + w] = 1 + d_from_s[w]
                    new_working.add(d + w)
    working = new_working

print(max(d_from_s.values()))

for initial_dir in pipedir["S"]:
    if spot + initial_dir in d_from_s:
        break
else:
    ValueError("No way out from start")


def phasediff(z1, z2):
    inc = phase(z1) - phase(z2)
    if inc > 3:
        inc -= 2 * cmath.pi
    elif inc < -3:
        inc += 2 * cmath.pi
    return inc


potential_areas = set(gridd) - set(d_from_s)
winding = {z: (phasediff(spot + initial_dir - z, spot - z)) for z in potential_areas}
nowat = spot + initial_dir
last_dir = initial_dir
i = 0
while nowat != spot:
    next_dir = [s for s in pipedir[gridd[nowat]] if s != -last_dir][0]
    for w in winding:
        inc = phasediff(nowat + next_dir - w, nowat - w)
        winding[w] += inc
    last_dir = next_dir
    nowat += next_dir
    i += 1
    if i % 100 == 0:
        print(i)

print(winding.values())
print()
print(len([wn for wn in winding.values() if wn > 2.5]))
