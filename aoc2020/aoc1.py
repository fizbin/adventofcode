#!/usr/bin/env python3
import sys
import re
import collections

with open('aoc1.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = [int(x) for x in list(f)]

sums1 = {}
sums2 = {}

for x in data:
    if (2020 - x) in sums2:
        print(x * sums2[2020-x])
    for y in sums1:
        sums2[x+y] = x * y
    sums1[x] = [x]
