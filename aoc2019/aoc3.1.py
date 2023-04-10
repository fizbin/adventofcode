#!/usr/bin/python
import sys
import re

with open('aoc3.1.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

spots = [set([]), set([])]
for line in (0, 1):
    spot = 0+0j
    for match in re.finditer(r'([UDLR])(\d+)', data[line]):
        one = {'U': 1j, 'D': -1j, 'L': -1, 'R': 1}[match.group(1)]
        for _ in range(int(match.group(2))):
            spot = spot + one
            spots[line].add(spot)

both = spots[0] & spots[1]

print(min(abs(x.real) + abs(x.imag) for x in both))
