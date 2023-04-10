#!/usr/bin/env python3
import sys
import re
import collections

with open('aoc2.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

policies = []
for line in data:
    m = re.match(r'(\d+)-(\d+) (\S): (\S+)', line)
    policies.append((int(m.group(1)), int(m.group(2)), m.group(3), m.group(4)))

def findall(p, s):
    i = s.find(p)
    while i != -1:
        yield i
        i = s.find(p, i+1)

nvalid = 0

for (minc, maxc, subs, passw) in policies:
    nfound = len(list(findall(subs, passw)))
    if minc <= nfound <= maxc:
        nvalid += 1

print(nvalid)

print()

nvalid = 0
for (minc, maxc, subs, passw) in policies:
    if ((passw[minc-1:minc] == subs) != (passw[maxc-1:maxc] == subs)):
        nvalid += 1
print(nvalid)

print()
