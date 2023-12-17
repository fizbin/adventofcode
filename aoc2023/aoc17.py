#!/usr/bin/python

import aoc_util
import heapq

data = aoc_util.get_data(17)
sgrid = aoc_util.chargrid(data)
width = len(sgrid[0])
height = len(sgrid)
# print(height, width)
grid = {(x, y): int(ch) for (x, row) in enumerate(sgrid) for (y, ch) in enumerate(row)}


def turn(_dir):
    return (-_dir[1], _dir[0])


def ingrid(where):
    return (0 <= where[0] < height) and (0 <= where[1] < width)


beenthere = set()
heap = []
heapq.heappush(heap, (grid[1, 0], 1, (1, 0), (1, 0)))
heapq.heappush(heap, (grid[0, 1], 1, (0, 1), (0, 1)))

# idx = 0
while heap:
    (sofar, n_in_row, pos, going) = heapq.heappop(heap)
    if pos == (height - 1, width - 1):
        break
    if (n_in_row, pos, going) in beenthere:
        continue
    beenthere.add((n_in_row, pos, going))
    # idx += 1
    # if idx % 1000 == 0:
    #     print("DBG: ", len(heap), sofar, pos)
    if n_in_row < 3:
        newpos = (pos[0] + going[0], pos[1] + going[1])
        if ingrid(newpos):
            heapq.heappush(heap, (sofar + grid[newpos], n_in_row + 1, newpos, going))
    ndir = turn(going)
    newpos = (pos[0] + ndir[0], pos[1] + ndir[1])
    if ingrid(newpos):
        heapq.heappush(heap, (sofar + grid[newpos], 1, newpos, ndir))
    ndir = turn(turn(ndir))
    newpos = (pos[0] + ndir[0], pos[1] + ndir[1])
    if ingrid(newpos):
        heapq.heappush(heap, (sofar + grid[newpos], 1, newpos, ndir))

print(sofar)  # , pos)


beenthere = set()
heap = []
heapq.heappush(heap, (grid[1, 0], 1, (1, 0), (1, 0)))
heapq.heappush(heap, (grid[0, 1], 1, (0, 1), (0, 1)))

# idx = 0
while heap:
    (sofar, n_in_row, pos, going) = heapq.heappop(heap)
    if pos == (height - 1, width - 1) and n_in_row >= 4:
        break
    if (n_in_row, pos, going) in beenthere:
        continue
    beenthere.add((n_in_row, pos, going))
    # idx += 1
    # if idx % 1000 == 0:
    #     print("DBG: ", len(heap), sofar, pos)
    if n_in_row < 10:
        newpos = (pos[0] + going[0], pos[1] + going[1])
        if ingrid(newpos):
            heapq.heappush(heap, (sofar + grid[newpos], n_in_row + 1, newpos, going))
    if n_in_row >= 4:
        ndir = turn(going)
        newpos = (pos[0] + ndir[0], pos[1] + ndir[1])
        if ingrid(newpos):
            heapq.heappush(heap, (sofar + grid[newpos], 1, newpos, ndir))
        ndir = turn(turn(ndir))
        newpos = (pos[0] + ndir[0], pos[1] + ndir[1])
        if ingrid(newpos):
            heapq.heappush(heap, (sofar + grid[newpos], 1, newpos, ndir))

print(sofar)
