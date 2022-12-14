from aoc_util import *
from functools import reduce, cmp_to_key
import json
import re
import numpy as np
import copy

data = get_data_lines(14)

grid = {}

maxy = 0
for line in data:
    spots = line.split("->")
    for (von, zu) in zip(spots, spots[1:]):
        (vonx, vony) = numbers(von)
        (zux, zuy) = numbers(zu)
        if vonx == zux:
            (a, b) = sorted([vony, zuy])
            for idx in range(a, b + 1):
                grid[(vonx, idx)] = "#"
        else:
            (a, b) = sorted([vonx, zux])
            for idx in range(a, b + 1):
                grid[(idx, vony)] = "#"
        maxy = max([maxy, vony, zuy])

og_grid = copy.deepcopy(grid)

grains = 0
while True:
    sandloc = (500, 0)
    alldone = False
    while True:
        if sandloc[1] > maxy:
            alldone = True
            break
        if grid.get((sandloc[0], sandloc[1] + 1)) is None:
            sandloc = (sandloc[0], sandloc[1] + 1)
            continue
        if grid.get((sandloc[0] - 1, sandloc[1] + 1)) is None:
            sandloc = (sandloc[0] - 1, sandloc[1] + 1)
            continue
        if grid.get((sandloc[0] + 1, sandloc[1] + 1)) is None:
            sandloc = (sandloc[0] + 1, sandloc[1] + 1)
            continue
        break
    if alldone:
        break
    grid[sandloc] = "o"
    grains += 1

print(grains)


grid = og_grid

for simx in range(-3000, 3000):
    grid[(simx, maxy + 2)] = "#"


maxy += 3

grains = 0
while True:
    sandloc = (500, 0)
    alldone = False
    while True:
        if sandloc[1] > maxy:
            alldone = True
            break
        if grid.get((sandloc[0], sandloc[1] + 1)) is None:
            sandloc = (sandloc[0], sandloc[1] + 1)
            continue
        if grid.get((sandloc[0] - 1, sandloc[1] + 1)) is None:
            sandloc = (sandloc[0] - 1, sandloc[1] + 1)
            continue
        if grid.get((sandloc[0] + 1, sandloc[1] + 1)) is None:
            sandloc = (sandloc[0] + 1, sandloc[1] + 1)
            continue
        break
    if alldone:
        assert False, "floor not wide enough"
    grid[sandloc] = "o"
    grains += 1
    if sandloc == (500, 0):
        break

print(grains)
