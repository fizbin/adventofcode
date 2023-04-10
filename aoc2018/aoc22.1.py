from __future__ import print_function

import sys
import re
import numpy as np

with open('aoc22.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

#

depth = int(re.findall('\d+', data[0])[0])
target = tuple(map(int, re.findall('\d+', data[1])))

# geologic = np.ndarray((target[0]+1, target[1]+1), dtype=np.uint64)
# geologic[:, 0] = range(target[0]+1)
# geologic[0, :] = range(target[1]+1)
# geologic[:, 0] *= 16807
# geologic[0, :] *= 48271

geologic = {}
for x in range(0, target[0]+1):
    for y in range(0, target[1]+1):
        if (x == 0):
            geologic[(x,y)] = y * 48271
        elif (y == 0):
            geologic[(x,y)] = x * 16807
        else:
            geologic[(x,y)] = (geologic[(x-1,y)]*geologic[(x,y-1)])
geologic[(0, 0)] = 0
geologic[target] = 0

tot = 0
for spot in geologic:
    erosion = (geologic[spot] + depth) % 20183
    terrain = erosion % 3
    tot += terrain

print(tot)
