#!/usr/bin/env python3
import sys
import re
import collections

with open('aoc1.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = sorted(set(int(x) for x in list(f)))

s = sum((1 << x) for x in data)
s2 = 0
lim = (1 << 2021) - 1
for x in data:
    s2 |= (s << x) & lim

inv = sum((1 << (2020-x)) for x in data)

intersect = inv & s2

prod = 1
bottom = 0
while intersect:
    if intersect & 1:
        print(2020 - bottom)
        prod *= 2020 - bottom
    bottom += 1
    intersect >>= 1

print()
print(prod)
