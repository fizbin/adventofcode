from __future__ import print_function

import sys
import re
import heapq

with open('aoc23.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

def d3(a, b):
    return abs(a[0]-b[0])+abs(a[1]-b[1])+abs(a[2]-b[2])

bots = [tuple(map(int, list(re.findall(r'-?\d+', ln)))) for ln in data]

maxrad = max(r for (x, y, z, r) in bots)
bot = [b for b in bots if b[3] == maxrad][0]

print(bot)
print(sum(1 for b in bots if d3(b, bot) <= maxrad))
