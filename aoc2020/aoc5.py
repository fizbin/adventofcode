#!/usr/bin/env python3
import sys

with open('aoc5.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(line.strip() for line in f)

maxval = 0
minval = 9999999
taken = set()
for seat in data:
    val = seat.replace('B', '1').replace('F', '0').replace('L', '0').replace('R', '1')
    val = int(val, 2)
    maxval = max(maxval, val)
    minval = min(minval, val)
    taken.add(val)

print(maxval)

print(min(set(range(minval, maxval+1)) - taken))

