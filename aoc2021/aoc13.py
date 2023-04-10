"""
Advent of code day 13 in python.
"""

import sys
import re


def dofold(axis, val, dots):
    val = int(val)
    if axis == "x":
        ndots = set(
            [(x, y) for (x, y) in dots if x < val]
            + [(2 * val - x, y) for (x, y) in dots if x > val]
        )
    else:
        ndots = set(
            [(x, y) for (x, y) in dots if y < val]
            + [(x, 2 * val - y) for (x, y) in dots if y > val]
        )
    return ndots


if __name__ == "__main__":
    with open("aoc13.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
        alldata = f.read()
        dots = re.findall(r"(\d+),(\d+)", alldata)
        dots = set([(int(x), int(y)) for (x, y) in dots])
        folds = re.findall(r"fold along ([xy])=(\d+)", alldata)
        # data = re.findall(r"\S+", f.read())
        # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
        # data = [tuple(int(y) for y in tup) for tup in data]

    ndots = dofold(folds[0][0], folds[0][1], dots)
    print(len(ndots))

    for (axis, val) in folds[1:]:
        ndots = dofold(axis, val, ndots)

    for col in range(max([v[1] for v in ndots]) + 1):
        for row in range(max([v[0] for v in ndots]) + 1):
            if (row, col) in ndots:
                print("#", end="")
            else:
                print(".", end="")
        print("")
