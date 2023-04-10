#!/usr/bin/env python
import sys
import re
import collections
import numpy as np
import itertools
import cmath

with open('aoc10.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

asters = set()
for (row, line) in enumerate(data):
    for (col, char) in enumerate(line):
        if char == '#':
            asters.add((col, row))


def visfrom(spotc, spotr):
    spotfoo = set()
    for (col, row) in asters:
        zcoord = (row - spotr) + (col - spotc)*(0+1j)
        if abs(zcoord) > 0:
            newnormed = cmath.phase(zcoord)
            if not any(cmath.isclose(newnormed, z) for z in spotfoo):
                spotfoo.add(newnormed)
    return len(spotfoo)


print("")
print(max((visfrom(col, row), (col, row)) for (col, row) in asters))
