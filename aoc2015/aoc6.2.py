import numpy as np
import sys
import re

with open('aoc6.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

lights = np.zeros((1000, 1000), dtype=int)

for ln in data:
    m = re.match(r'turn on (\d+), *(\d+) through (\d+), *(\d+)', ln)
    if m:
        (x1, y1, x2, y2) = map(int, m.groups())
        lights[x1:x2+1, y1:y2+1] += 1
        continue
    m = re.match(r'turn off (\d+), *(\d+) through (\d+), *(\d+)', ln)
    if m:
        (x1, y1, x2, y2) = map(int, m.groups())
        lights[x1:x2+1, y1:y2+1] -= 1
        lights[x1:x2+1, y1:y2+1] = np.maximum(lights[x1:x2+1, y1:y2+1], 0)
        continue
    m = re.match(r'toggle (\d+), *(\d+) through (\d+), *(\d+)', ln)
    if m:
        (x1, y1, x2, y2) = map(int, m.groups())
        lights[x1:x2+1, y1:y2+1] += 2
        continue
    raise Exception(repr(ln))

print(lights.sum())

