#!/usr/bin/python

import aoc_util
import numpy as np

data = aoc_util.get_data(23)
grid = aoc_util.chargrid(data)
grid_d = {(i,j):ch for (i, row) in enumerate(grid) for (j, ch) in enumerate(row)}

height = len(grid)

maxlen = 0
workq = [((0,1),frozenset([(0,1)]))]
while workq:
    (current_loc, visited) = workq.pop()
    if (current_loc[0] == height - 1):
        maxlen = max(maxlen, len(visited))
        continue
    nbs = [(current_loc[0]-1, current_loc[1]), (current_loc[0], current_loc[1]-1), (current_loc[0]+1, current_loc[1]),(current_loc[0],current_loc[1]+1)]
    for nb in nbs:
        if nb[0] < 0:
            continue
        if grid_d[nb] == '#' or nb in visited:
            continue
        if grid_d[nb] == '>':
            if nb != (current_loc[0],current_loc[1]+1):
                continue
        elif grid_d[nb] == 'v':
            if nb != (current_loc[0]+1,current_loc[1]):
                continue
        elif grid_d[nb] == '<':
            if nb != (current_loc[0],current_loc[1]-1):
                continue
        elif grid_d[nb] == '^':
            if nb != (current_loc[0]-1,current_loc[1]):
                continue
        elif grid_d[nb] != '.':
            continue
        workq.append((nb, visited | set([nb])))

print(maxlen - 1)

maxlen = 0
step = 0
workq = [((0,1),frozenset([(0,1)]))]
while workq:
    (current_loc, visited) = workq.pop()
    step += 1
    if (current_loc[0] == height - 1):
        maxlen = max(maxlen, len(visited))
        continue
    nbs = [(current_loc[0]-1, current_loc[1]), (current_loc[0], current_loc[1]-1), (current_loc[0]+1, current_loc[1]),(current_loc[0],current_loc[1]+1)]
    cont_spots = 0
    for nb in nbs:
        if nb[0] < 0:
            continue
        if grid_d[nb] == '#' or nb in visited:
            continue
        if grid_d[nb] not in 'v><^.':
            continue
        workq.append((nb, visited | set([nb])))
        cont_spots += 1
    # if cont_spots > 1:
    #     print("DBG", current_loc)

print(maxlen - 1)
