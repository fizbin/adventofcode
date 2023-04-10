#!/usr/bin/env python
import sys
import re

with open('aoc6.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

orbits = {}
for line in data:
    m = re.match(r'(\w+)\)(\w+)$', line)
    orbits.setdefault(m.group(1), set()).add(m.group(2))

ocounts = {'COM': 0}

def follow_thing(center):
    for outer in orbits.get(center, set()):
        ocounts[outer] = ocounts[center] + 1
    for outer in orbits.get(center, set()):
        follow_thing(outer)

follow_thing('COM')
print(sum(ocounts.values()))

