#!/usr/bin/env python3
import sys
import re

with open('aoc12.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

pos = 0+0j
head = 1j
for line in data:
    thing = line[0]
    dist = int(line[1:])
    if thing == 'N':
        pos += dist
    elif thing == 'S':
        pos -= dist
    elif thing == 'E':
        pos += 1j * dist
    elif thing == 'W':
        pos -= 1j * dist
    elif thing == 'F':
        pos += head * dist
    elif thing == 'R':
        while dist > 0:
            head *= 1j
            dist -= 90
    elif thing == 'L':
        while dist > 0:
            head *= -1j
            dist -= 90

print(abs(pos.real) + abs(pos.imag))
