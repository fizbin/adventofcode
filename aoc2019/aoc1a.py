#!/usr/bin/env python
import sys

with open('aoc1a.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

print(sum((int(x) // 3) - 2 for x in data))
