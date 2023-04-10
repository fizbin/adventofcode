#!/usr/bin/env python3
import sys
import re

with open('aoc6.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read().split('\n\n')

mycount = 0
for group in data:
    myset = set(re.findall(r'\w', group))
    for line in group.split('\n'):
        myset = myset & set(re.findall(r'\w', line))
    mycount += len(myset)

print(mycount)
