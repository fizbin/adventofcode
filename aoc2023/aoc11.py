#!/usr/bin/python

import aoc_util
import itertools

data = aoc_util.get_data(11)
grid = aoc_util.chargrid(data)

empty_rows = [idx for idx in range(len(grid)) if all(ch == "." for ch in grid[idx])]
empty_cols = [idx for idx in range(len(grid[0])) if all(row[idx] == "." for row in grid)]


gal_coords = [
    (rowidx, colidx)
    for (rowidx, row) in enumerate(grid)
    for (colidx, ch) in enumerate(row)
    if ch == "#"
]

total = 0
for gal1, gal2 in itertools.combinations(gal_coords, 2):
    distance = abs(gal1[0] - gal2[0]) + abs(gal1[1] - gal2[1])
    distance += len([er for er in empty_rows if gal1[0] < er < gal2[0] or gal2[0] < er < gal1[0]])
    distance += len([ec for ec in empty_cols if gal1[1] < ec < gal2[1] or gal2[1] < ec < gal1[1]])
    total += distance
print(total)

total = 0
for gal1, gal2 in itertools.combinations(gal_coords, 2):
    distance = abs(gal1[0] - gal2[0]) + abs(gal1[1] - gal2[1])
    distance += 999999 * len(
        [er for er in empty_rows if gal1[0] < er < gal2[0] or gal2[0] < er < gal1[0]]
    )
    distance += 999999 * len(
        [ec for ec in empty_cols if gal1[1] < ec < gal2[1] or gal2[1] < ec < gal1[1]]
    )
    total += distance
print(total)
