import sys
import re
import itertools

with open('aoc5.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

horizlines = [(int(x1),int(y1),int(x2),int(y2)) for (x1,y1,x2,y2) in data if x1 == x2]
vertlines = [(int(x1),int(y1),int(x2),int(y2)) for (x1,y1,x2,y2) in data if y1 == y2]

covered = {}
for (x1,y1,x2,y2) in horizlines:
    (ya, yb) = sorted([y1,y2])
    for y in range(ya, yb+1):
        covered[(x1, y)] = covered.get((x1,y),0) + 1
for (x1,y1,x2,y2) in vertlines:
    (xa, xb) = sorted([x1,x2])
    for x in range(xa, xb+1):
        covered[(x, y1)] = covered.get((x,y1),0) + 1

count = 0
for spot in covered:
    if covered[spot] > 1:
        count += 1

print(count)

diaglines = [(int(x1),int(y1),int(x2),int(y2)) for (x1,y1,x2,y2) in data if y1 != y2 and x1 != x2]

for (x1,y1,x2,y2) in diaglines:
    dir1 = 1 if x1 < x2 else -1
    dir2 = 1 if y1 < y2 else -1
    for dist in range(abs(x1-x2) + 1):
        spot = (x1 + dir1*dist, y1 + dir2*dist)
        covered[spot] = covered.get(spot,0)+1

count = 0
for spot in covered:
    if covered[spot] > 1:
        count += 1

print(count)