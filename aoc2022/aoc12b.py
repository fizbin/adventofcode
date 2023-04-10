"""Advent of Code 2022 day 12"""

from aoc_util import chargrid, get_data, astar

# pylint: disable=invalid-name

data = chargrid(get_data(12))

start = None
for row in range(len(data)):
    for col in range(len(data[0])):
        if data[row][col] == "S":
            start = (row, col)
            break
    if start is not None:
        break


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
        if ord(t) <= ord(curr) + 1:
            retval2.append((r2, c2))
    return retval2


(d, path) = astar(
    start=start, goalf=lambda p: data[p[0]][p[1]] == "E", neighborf=neighborf
)
print(d, path[0], path[-1])

(d, path) = astar(
    starts=[
        (row, col)
        for row in range(len(data))
        for col in range(len(data[0]))
        if data[row][col] in "Sa"
    ],
    goalf=lambda p: data[p[0]][p[1]] == "E",
    neighborf=neighborf,
)
print(d, path[0], path[-1])
