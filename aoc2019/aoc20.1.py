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

labelsum = {}
for (zspot, ch) in maze.items():
    if ch in string.ascii_uppercase:
        for zdir in (-1, 1, 1j, -1j):
            if maze.get(zspot + zdir, ' ') == '.':
                label = maze[zspot - zdir] + ch
                if zdir in (-1, -1j):
                    label = ''.join(reversed(label))
                labelsum[label] = labelsum.get(label, 0) + zspot + zdir

repl = {}
for (zspot, ch) in maze.items():
    if ch in string.ascii_uppercase:
        for zdir in (-1, 1, 1j, -1j):
            if maze.get(zspot + zdir, ' ') == '.':
                label = maze[zspot - zdir] + ch
                if zdir in (-1, -1j):
                    label = ''.join(reversed(label))
                otherspot = labelsum[label] - zspot - zdir
                if otherspot != 0:
                    repl[zspot] = otherspot

start = labelsum['AA']
goal = labelsum['ZZ']
best = {}
workq = queue.Queue()
workq.put((0, start))
while not workq.empty():
    (dist, zspot) = workq.get()
    if zspot == goal:
        print("Distance", dist)
        break
    if zspot in best and best[zspot] <= dist:
        continue
    best[zspot] = dist
    for nextz in (zspot + 1, zspot - 1, zspot + 1j, zspot - 1j):
        if nextz in repl:
            nextz = repl[nextz]
        if maze[nextz] != '.':
            continue
        if nextz not in best or best[nextz] > dist+1:
            workq.put((dist+1, nextz))
