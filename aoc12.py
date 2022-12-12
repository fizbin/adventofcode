from aoc_util import *
from functools import reduce
import copy
import re
import numpy as np

data = chargrid(get_data(12))

start = None
end = None
for row in range(len(data)):
    for col in range(len(data[0])):
        if data[row][col] == "S":
            start = (row, col)
            break
    if start is not None:
        break


def make_neighborf(part1):
    def neighborf(p):
        (r, c) = p
        retval = []
        if r > 0:
            retval.append((r - 1, c))
        if c > 0:
            retval.append((r, c - 1))
        if r < len(data) - 1:
            retval.append((r + 1, c))
        if c < len(data[0]) - 1:
            retval.append((r, c + 1))
        curr = data[r][c]
        if curr == "S":
            curr = "a"
        if curr == "E":
            curr = "z"
        retval2 = []
        for (r2, c2) in retval:
            t = data[r2][c2]
            if t == "S":
                t = "a"
            if t == "E":
                t = "z"
            if (part1 and ord(t) <= ord(curr) + 1) or (
                not part1 and ord(t) >= ord(curr) - 1
            ):
                retval2.append((r2, c2))
        return [(p, 1) for p in retval2]

    return neighborf


(d, path) = astar(start, lambda p: data[p[0]][p[1]] == "E", make_neighborf(True))
print(d)

(d, path) = astar(path[-1], lambda p: data[p[0]][p[1]] in "aS", make_neighborf(False))
print(d)
