#!/usr/bin/env python3
import sys
from functools import reduce
from operator import xor

with open('aoc5.in' if len(sys.argv) < 2 else sys.argv[1], 'rb') as f:
    data = [int(bytes(48 + int(not (x&(x>>1))) for x in line.strip()), 2)
            for line in f]

maxval = max(data)
minval = min(data)
cx = lambda n: (n & (n << 1 & 2) - 1) ^ (n >> 1 & 1)
print(maxval)
print(cx(maxval) ^ cx(minval - 1) ^ reduce(xor, data))
