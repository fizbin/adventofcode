#!/usr/bin/env python
import sys
import re

with open('aoc6.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

orbits = {}
rorbits = {}
for line in data:
    m = re.match(r'(\w+)\)(\w+)$', line)
    orbits.setdefault(m.group(1), set()).add(m.group(2))
    rorbits[m.group(2)] = m.group(1)

d_to_santa = {'SAN': 0}


def follow_thing(center):
    neighbors = set(orbits.get(center, []))
    if center in rorbits:
        neighbors.add(rorbits[center])
    my_d = d_to_santa[center]
    to_visit = set()
    for n in neighbors:
        if d_to_santa.get(n, my_d + 5) > my_d + 1:
            d_to_santa[n] = my_d + 1
            to_visit.add(n)
    for n in to_visit:
        follow_thing(n)


follow_thing('SAN')
print(d_to_santa['YOU'] - 2)
