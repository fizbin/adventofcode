from aoc_util import *
import numpy as np
import re

data = get_data_lines(9)

dataee = '''
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
'''.strip().splitlines()
visited = set()

def new_pos(hd, tl):
    if abs(hd - tl) > 1.5:
        if abs(hd - tl) == 2.0:
            tl = (hd + tl) / 2
        else:
            offset = 1+1j
            poss = [tl + offset, tl - offset, tl + offset*1j, tl - offset*1j]
            tl = min(poss, key=lambda x: abs(hd - x))
    return tl

hd = 0+0j
tl = hd
for (direction, multi) in [li.split() for li in data]:
    unit = {'U': 1, 'D': -1, 'L': 1j, 'R': -1j}[direction]
    multi = int(multi)
    for _ in range(multi):
        hd += unit
        tl = new_pos(hd, tl)
        visited.add(tl)

print(len(visited))

visited = set()
spots = [0+0j] * 10
for (direction, multi) in [li.split() for li in data]:
    unit = {'U': 1, 'D': -1, 'L': 1j, 'R': -1j}[direction]
    multi = int(multi)
    for _ in range(multi):
        spots[0] += unit
        for idx in range(1,10):
            spots[idx] = new_pos(spots[idx-1], spots[idx])
        visited.add(spots[9])

print(len(visited))