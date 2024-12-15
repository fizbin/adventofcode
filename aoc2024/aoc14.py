from aoc_util import get_data_lines, get_data, get_data_paras
import numpy as np
import scipy.ndimage
import itertools
import re
import copy


data = get_data_lines(14)

irobots = []
frobots = []
quads = {1: 0, 2: 0, 3: 0, 4: 0, 0: 0}
for line in data:
    m = re.match(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)", line)
    (x, y, vx, vy) = map(int, m.groups())
    irobots.append((x, y, vx, vy))
    (fx, fy) = ((x + 100 * vx) % 101, (y + 100 * vy) % 103)
    frobots.append(((x + 100 * vx) % 101, (y + 100 * vy) % 103))
    quad = 0
    if fx < 101 // 2 and fy < 103 // 2:
        quad = 1
    if fx > 101 // 2 and fy < 103 // 2:
        quad = 2
    if fx < 101 // 2 and fy > 103 // 2:
        quad = 3
    if fx > 101 // 2 and fy > 103 // 2:
        quad = 4
    quads[quad] += 1

print("Part 1:", quads[1] * quads[2] * quads[3] * quads[4], flush=True)


class FoundTree(Exception):
    pass


def print_grid(gen, robots):
    tgt = "##########"
    grid = [["."] * 101 for _ in range(103)]
    for x, y, _, _ in robots:
        grid[y][x] = "#"
    grid = ["".join(x) for x in grid]
    if any(tgt in x for x in grid):
        print(f"Generation {gen}:")
        list(map(lambda x: print(x), grid))
        raise (FoundTree(gen))


robots = irobots
gen = 0
try:
    while True:
        print_grid(gen, robots)
        nrobots = map(lambda t: ((t[0] + t[2]) % 101, (t[1] + t[3]) % 103, t[2], t[3]), robots)
        robots = list(nrobots)
        gen += 1
except FoundTree as e:
    print("Part 2:", e.args[0])
