import sys
import collections
import string
import heapq
import queue

with open('aoc20.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

start = None
keys = {}
maze = {}
for (y, line) in enumerate(data):
    for (x, ch) in enumerate(line):
        maze[y + (0+1j)*x] = ch

maxreal = max(z.real for z in maze)
maximag = max(z.imag for z in maze)

labelsum = {}
for (zspot, ch) in maze.items():
    if ch in string.ascii_uppercase:
        for zdir in (-1j, -1, 1j, 1):
            if maze.get(zspot + zdir, ' ') == '.':
                label = maze[zspot - zdir] + ch
                if zdir in (-1, -1j):
                    label = ''.join(reversed(label))
                labelsum[label] = labelsum.get(label, 0) + zspot + zdir

repl = {}
for (zspot, ch) in maze.items():
    if ch in string.ascii_uppercase:
        for zdir in (-1j, -1, 1j, 1):
            if maze.get(zspot + zdir, ' ') == '.':
                label = maze[zspot - zdir] + ch
                if zdir in (-1, -1j):
                    label = ''.join(reversed(label))
                otherspot = labelsum[label] - zspot - zdir
                if otherspot != 0:
                    repl[zspot] = otherspot

for replz in list(repl):
    if (replz.imag < 3 or
        replz.real < 3 or
        replz.imag > maximag - 4 or
            replz.real > maximag - 4):
        repl[replz] = (repl[replz], -1)
    else:
        repl[replz] = (repl[replz], 1)

start = labelsum['AA']
goal = labelsum['ZZ']
print("start",start,"goal",goal)
best = {}
workq = queue.Queue()
workq.put((0, (start, 0)))
printed = 0
while not workq.empty():
    (dist, (zspot, zlevel)) = workq.get()
    if printed < 5000:
        print("At", (zlevel, zspot))
        printed += 1
    if zspot == goal and zlevel == 0:
        print("Distance", dist)
        break
    best[(zspot, zlevel)] = dist
    for nextz in (zspot - 1j, zspot - 1, zspot + 1j, zspot + 1):
        nextzlevel = zlevel
        if nextz in repl:
            (nextz, levelinc) = repl[nextz]
            nextzlevel += levelinc
            if nextzlevel < 0:
                continue
        if maze[nextz] != '.':
            continue
        if ((nextz, nextzlevel) not in best or
                best[(nextz, nextzlevel)] > dist+1):
            workq.put((dist+1, (nextz, nextzlevel)))
