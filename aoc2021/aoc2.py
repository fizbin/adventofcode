#!/usr/bin/env python3
import sys
import re
import collections

data = []
with open('aoc2.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    for line in f:
        (a, b) = line.split(' ')
        data.append((a,b))

horiz = 0
depth = 0
for (a, b) in data:
    if a == 'forward':
        horiz += int(b)
    elif a == 'up':
        depth -= int(b)
    elif a == 'down':
        depth += int(b)
    else:
        raise Exception(f"uh-oh {(a,b)}")

print(horiz*depth)

horiz = 0
depth = 0
aim = 0
for (a, b) in data:
    if a == 'forward':
        horiz += int(b)
        depth += aim*int(b)
    elif a == 'up':
        aim -= int(b)
    elif a == 'down':
        aim += int(b)
    else:
        raise Exception(f"uh-oh {(a,b)}")
print(horiz*depth)
