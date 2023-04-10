"""
Advent of code 2021 day 21, part one in python.
"""

import sys
import re
import collections
import functools
import itertools

DIE = [1]


def do_roll():
    val = DIE[0]
    DIE[0] = val + 1
    return 1 + ((val - 1) % 1000)


if __name__ == "__main__":
    with open("aoc21.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
        # data = re.findall(r"(\w+)-(\w+)", f.read())
        # data = re.findall(r"(-?\d+)\.\.(-?\d+)", f.read())
        # data = re.findall(r"scanner [^s]*", f.read())
        data = re.findall(r"\d+", f.read())
        # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
        # data = [tuple(int(y) for y in tup) for tup in data]
        start1 = int(data[1])
        start2 = int(data[3])
    spot1 = start1
    spot2 = start2
    score1 = score2 = 0
    while (score1 < 1000) and (score2 < 1000):
        spot1 += do_roll() + do_roll() + do_roll()
        while spot1 > 10:
            spot1 -= 10
        score1 += spot1
        if score1 >= 1000:
            break
        spot2 += do_roll() + do_roll() + do_roll()
        while spot2 > 10:
            spot2 -= 10
        score2 += spot2
    print((DIE[0] - 1) * min(score1, score2))
