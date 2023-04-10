"""
Advent of code 2021 day 25 in python.
"""

import sys
import re
import numpy as np

if __name__ == "__main__":
    with open("aoc25.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        data = re.findall(
            r"\S+",
            f.read(),
        )
        grid = [re.findall(r'\S', line) for line in data]
    grid = np.array(grid, dtype='U1')
    daynum = 0
    changed = True
    while changed:
        daynum += 1
        changed = False
        grid1 = np.roll(grid, 1, 1)
        gridm1 = np.roll(grid, -1, 1)
        hmove = np.where((grid == '.') & (grid1 == '>'), '>', np.where((grid == '>') & (gridm1 == '.'), '.', grid))

        grid1 = np.roll(hmove, 1, 0)
        gridm1 = np.roll(hmove, -1, 0)
        vmove = np.where((hmove == '.') & (grid1 == 'v'), 'v', np.where((hmove == 'v') & (gridm1 == '.'), '.', hmove))
        changed = np.any(vmove != grid)
        grid = vmove
        # outp = "\x1B[0;0H"
        # for row in grid[:140]:
        #     outp += ''.join(row) + '\n'
        # print(outp)
    print(daynum)