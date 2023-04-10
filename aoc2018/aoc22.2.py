from __future__ import print_function

import sys
import re
import heapq

with open('aoc22.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

depth = int(re.findall('\d+', data[0])[0])
target = tuple(map(int, re.findall('\d+', data[1])))

print(depth)
print(target)

geologic = {}
for x in range(0, target[0]+130):
    for y in range(0, target[1]+20):
        if (x == 0):
            geologic[(x, y)] = y * 48271
        elif (y == 0):
            geologic[(x, y)] = x * 16807
        else:
            # wasted time on part 1 by not reading carefully: you multiply
            # *erosion* levels, not *geologic* indexes. The side effect is
            # that I needed to add depth in twice here:
            geologic[(x, y)] = (
                (geologic[(x-1, y)]+depth)*(geologic[(x, y-1)]+depth)) % 20183
geologic[(0, 0)] = 0
geologic[target] = 0

tot = 0
terrain = {}
for spot in geologic:
    erosion = (geologic[spot] + depth) % 20183
    terrain[spot] = erosion % 3
    if spot[0] <= target[0] and spot[1] <= target[1]:
        tot += terrain[spot]

print("Part 1:", tot)
# estimate, time-so-far, x, y, item
# item is 0, 1, or 2 - 0 means "neither", 1 means "light", 2 means "climbing gear"
# that numbering system means that the terrain/gear rules work out to
# "item X cannoth be used in terrain X"
workheap = [(target[0] + target[1], 0, 0, 0, 1)]
besttime = {}
while workheap:
    (_, sofar, x, y, item) = heapq.heappop(workheap)
    # print(sofar)
    if (x, y) == target and item == 1:
        print("Part 2:", sofar)
        break
    neighbors = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    for n in neighbors:
        if n in terrain:
            if item != terrain[n]:
                if besttime.get((n[0], n[1], item), sofar + 999) > sofar + 1:
                    estimate = abs(n[0] - target[0]) + abs(n[1] - target[1])
                    if item != 1:
                        estimate += 7
                    heapq.heappush(
                        workheap, (estimate + sofar + 1, sofar + 1,
                                   n[0], n[1], item))
                    besttime[(n[0], n[1], item)] = sofar + 1
        else:
            if n[0] >= 0 and n[1] >= 0:
                print("Tried to look at %s but couldn't" % (n,))
    for it in range(3):
        if it != terrain[(x, y)] and it != item:
            if besttime.get((x, y, it), sofar + 999) > sofar + 7:
                estimate = abs(x - target[0]) + abs(y - target[1])
                if it != 1:
                    estimate += 7
                heapq.heappush(
                    workheap, (estimate + sofar + 7, sofar + 7, x, y, it))
                besttime[(x, y, it)] = sofar + 7
