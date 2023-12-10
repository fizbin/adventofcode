#!/usr/bin/env python

from itertools import pairwise
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
nowat = spot + initial_dir
chain = [spot]
last_dir = initial_dir
i = 0
while nowat != spot:
    chain.append(nowat)
    next_dir = [s for s in pipedir[gridd[nowat]] if s != -last_dir][0]
    last_dir = next_dir
    nowat += next_dir

potential_areas = set(gridd) - set(d_from_s)
winding = {z: 0 for z in potential_areas}
pot_rows = {}
for p in potential_areas:
    pot_rows.setdefault(p.real, []).append(p)

for von, zu in pairwise(chain + [chain[0]]):
    step = zu - von
    if step.imag != 0:
        continue
    if step.real < 0:
        for p in pot_rows.get(von.real, []):
            if p.imag < von.imag:
                winding[p] += 1
    if step.real > 0:
        for p in pot_rows.get(zu.real, []):
            if p.imag < zu.imag:
                winding[p] -= 1

print(len([w for w in winding.values() if w != 0]))
