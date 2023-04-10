#!/usr/bin/env python
import sys

with open('aoc1a.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)


def totfuel(x):
    a = (int(x) // 3) - 2
    if a > 0:
        return a + totfuel(a)
    return 0


print(sum(totfuel(x) for x in data))
