#!/usr/bin/python
import sys
import re

with open('aoc3.1.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

spots = [set(), set()]
lento = [{}, {}]
for line in (0, 1):
    spot = 0+0j
    steps = 0
    for match in re.finditer(r'([UDLR])(\d+)', data[line]):
        one = {'U': 1j, 'D': -1j, 'L': -1, 'R': 1}[match.group(1)]
        for _ in range(int(match.group(2))):
            spot = spot + one
            steps += 1
            spots[line].add(spot)
            lento[line].setdefault(spot, steps)

both = spots[0] & spots[1]

print(min(lento[0][x] + lento[1][x] for x in both))
