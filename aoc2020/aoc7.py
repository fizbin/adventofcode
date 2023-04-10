#!/usr/bin/env python3
import sys
import re

with open('aoc7.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

forward = {}
reverse = {}
for line in data:
    m = re.match(r'(\w+\s\w+) bags contain (.*)\.', line)
    srcbag = m.group(1)
    dstbags = []
    if m.group(2) != 'no other bags':
        for m2 in re.finditer(r'(\d+) (\w+\s\w+) bags?', m.group(2)):
            dstbags.append((int(m2.group(1)), m2.group(2)))
    forward[srcbag] = dstbags
    for (quant, bagtype) in dstbags:
        reverse.setdefault(bagtype, set()).add(srcbag)

# part A
bagset = set(reverse.get('shiny gold', set()))
bagset1 = set()
while bagset != bagset1:
    bagset1 = set(bagset)
    for color in bagset1:
        for revcolor in reverse.get(color, set()):
            bagset.add(revcolor)

print(len(bagset))

# part B

def bagcontents(color):
    total = 0
    for (mult, contcolor) in forward[color]:
        total += mult*(1 + bagcontents(contcolor))
    return total

print(bagcontents('shiny gold'))
