"""
Advent of code 2021 day 25 in python.
"""

import sys
import re

if __name__ == "__main__":
    with open("aoc25.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        data = re.findall(
            r"\S+",
            f.read(),
        )
        grid = [re.findall(r'\S', line) for line in data]
    rgtrange = list(zip(range(len(grid[0])), range(1, len(grid[0])))) + [(len(grid[0])-1, 0)]
    dwnrange = list(zip(range(len(grid)), range(1, len(grid)))) + [(len(grid)-1, 0)]
    daynum = 0
    changed = True
    while changed:
        daynum += 1
        changed = False
        for (idx, row) in enumerate(grid):
            for (fst, snd) in rgtrange:
                if grid[idx][fst] + grid[idx][snd] == '>.':
                    grid[idx][fst] = 'M'
            for (fst, snd) in rgtrange:
                if grid[idx][fst] + grid[idx][snd] == 'M.':
                    grid[idx][fst] = '.'
                    grid[idx][snd] = '>'
                    changed = True
        for idx in range(len(grid[0])):
            for (fst, snd) in dwnrange:
                if grid[fst][idx] + grid[snd][idx] == 'v.':
                    grid[fst][idx] = 'M'
            for (fst, snd) in dwnrange:
                if grid[fst][idx] + grid[snd][idx] == 'M.':
                    grid[fst][idx] = '.'
                    grid[snd][idx] = 'v'
                    changed = True
    print(daynum)