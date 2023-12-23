#!/usr/bin/python

import aoc_util
import heapq

data = aoc_util.get_data(23)
grid = aoc_util.chargrid(data)
grid_d = {(i,j):ch for (i, row) in enumerate(grid) for (j, ch) in enumerate(row)}

height = len(grid)

# maxlen = 0
# workq = [((0,1),frozenset([(0,1)]))]
# while workq:
#     (current_loc, visited) = workq.pop()
#     if (current_loc[0] == height - 1):
#         maxlen = max(maxlen, len(visited))
#         continue
#     nbs = [(current_loc[0]-1, current_loc[1]), (current_loc[0], current_loc[1]-1), (current_loc[0]+1, current_loc[1]),(current_loc[0],current_loc[1]+1)]
#     for nb in nbs:
#         if nb[0] < 0:
#             continue
#         if grid_d[nb] == '#' or nb in visited:
#             continue
#         if grid_d[nb] == '>':
#             if nb != (current_loc[0],current_loc[1]+1):
#                 continue
#         elif grid_d[nb] == 'v':
#             if nb != (current_loc[0]+1,current_loc[1]):
#                 continue
#         elif grid_d[nb] == '<':
#             if nb != (current_loc[0],current_loc[1]-1):
#                 continue
#         elif grid_d[nb] == '^':
#             if nb != (current_loc[0]-1,current_loc[1]):
#                 continue
#         elif grid_d[nb] != '.':
#             continue
#         workq.append((nb, visited | set([nb])))

# print(maxlen - 1)

choice_spots = {}

for (x, row) in enumerate(grid):
    for (y, ch) in enumerate(row):
        if ch == '#':
            continue
        if x == 0 or x == height - 1:
            continue
        nbs = [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]
        good_nbs = [nb for nb in nbs if grid_d[nb] != '#']
        if len(good_nbs) > 2:
            choice_spots[(x,y)] = good_nbs

choice_grid = {}
for nexus in choice_spots:
    this_toward = []
    for nb in choice_spots[nexus]:
        visited = set([nexus, nb])
        while nb not in choice_spots and 0 < nb[0] < height - 1:
            (x,y) = nb
            new_nbs = [(x-1,y),(x,y-1),(x+1,y),(x,y+1)]
            good_nbs = [n for n in new_nbs if grid_d[n] != '#' and n not in visited]
            assert (len(good_nbs) == 1), f"At {nb}, visited is {visited}, good is {good_nbs}"
            nb = good_nbs[0]
            visited.add(nb)
        if nb[0] == 0 or nb[0] == height-1:
            choice_grid[nb] = [(nexus, len(visited)-1)]
        this_toward.append((nb, len(visited) - 1))
    choice_grid[nexus] = this_toward

print(choice_grid)
print(len(choice_grid))
print(sum((2 ** len(choice_grid[xus])) for xus in choice_grid))
print(sum(1 for xus in choice_grid if len(choice_grid[xus]) == 4))

been_here = {}
workq = [(0, (0,1), frozenset([0,1]))]
maxlen = 0
while workq:
    (neg_sofar, pos, visited) = heapq.heappop(workq)
    if (pos, visited) in been_here:
        if been_here[(pos, visited)] <= neg_sofar:
            # already have longer path
            continue
    been_here[(pos, visited)] = neg_sofar
    if pos[0] == height - 1:
        maxlen = max(maxlen, -neg_sofar)
        print("DBG", maxlen)
    for (dest_pos, dest_d) in choice_grid[pos]:
        if dest_pos not in visited:
            heapq.heappush(workq, (neg_sofar - dest_d, dest_pos, visited | frozenset([dest_pos])))

print(maxlen)
    