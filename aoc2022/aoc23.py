from aoc_util import *
from collections import Counter
import copy

def elfstep(elves, tick):
    dirtrials = [-1+0j, 1+0j, 0-1j, 0+1j,-1+0j, 1+0j, 0-1j, 0+1j][tick%4:4+(tick%4)]
    elfdest = {}
    for elf in elves:
        neighbors = set([elf + d for d in [-1-1j, -1, -1+1j, 0-1j, 0+1j, 1-1j, 1, 1+1j]])
        if not (neighbors & elves):
            continue
        for d in dirtrials:
            dneigh = set([elf + dp for dp in [(1+1j)*d, d, (1-1j)*d]])
            if not (dneigh & elves):
                elfdest[elf] = elf + d
                break
    dests = Counter(elfdest.values())
    newelves = set()
    done = True
    for elf in elves:
        edest = elfdest.get(elf, elf)
        if dests[edest] > 1:
            edest = elf
        newelves.add(edest)
        if edest != elf:
            done = False

    # minx = int(min(e.real for e in newelves))
    # maxx = int(max(e.real for e in newelves))
    # miny = int(min(e.imag for e in newelves))
    # maxy = int(max(e.imag for e in newelves))

    # for r in range(minx,maxx+1):
    #     for c in range(miny,maxy+1):
    #         if r+c*1j in newelves:
    #             print('#', end='')
    #         else:
    #             print('.', end='')
    #     print()
    # print()

    if done:
        return None
    return newelves

data = chargrid(get_data(23))

elves = set()
for (row, line) in enumerate(data):
    for (col, ch) in enumerate(line):
        if ch == '#':
            elves.add(row + col*1j)
og_elves = copy.deepcopy(elves)

for tick in range(10):
    elves = elfstep(elves, tick)

minx = int(min(e.real for e in elves))
maxx = int(max(e.real for e in elves))
miny = int(min(e.imag for e in elves))
maxy = int(max(e.imag for e in elves))

print((maxx-minx+1)*(maxy-miny+1) - len(elves))

elves = og_elves
tick = 1
while True:
    elves = elfstep(elves, tick-1)
    if elves is None:
        print(tick)
        break
    tick += 1
