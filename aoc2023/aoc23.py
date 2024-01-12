#!/usr/bin/python

import aoc_util
import heapq

data = aoc_util.get_data(23)
grid = aoc_util.chargrid(data)
grid_d = {(i, j): ch for (i, row) in enumerate(grid) for (j, ch) in enumerate(row)}

height = len(grid)


def p1() -> int:
    maxlen = 0
    workq = [((0, 1), frozenset([(0, 1)]))]
    while workq:
        (current_loc, visited) = workq.pop()
        if current_loc[0] == height - 1:
            maxlen = max(maxlen, len(visited) - 1)
            continue
        nbs = [
            (current_loc[0] - 1, current_loc[1]),
            (current_loc[0], current_loc[1] - 1),
            (current_loc[0] + 1, current_loc[1]),
            (current_loc[0], current_loc[1] + 1),
        ]
        for nb in nbs:
            if nb[0] < 0:
                continue
            if grid_d[nb] == "#" or nb in visited:
                continue
            if grid_d[nb] == ">":
                if nb != (current_loc[0], current_loc[1] + 1):
                    continue
            elif grid_d[nb] == "v":
                if nb != (current_loc[0] + 1, current_loc[1]):
                    continue
            elif grid_d[nb] == "<":
                if nb != (current_loc[0], current_loc[1] - 1):
                    continue
            elif grid_d[nb] == "^":
                if nb != (current_loc[0] - 1, current_loc[1]):
                    continue
            elif grid_d[nb] != ".":
                continue
            workq.append((nb, visited | set([nb])))
    return maxlen


print(p1())

choice_spots = {}

for x, row in enumerate(grid):
    for y, ch in enumerate(row):
        if ch == "#":
            continue
        if x == 0 or x == height - 1:
            continue
        nbs = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
        good_nbs = [nb for nb in nbs if grid_d[nb] != "#"]
        if len(good_nbs) > 2:
            choice_spots[(x, y)] = good_nbs

choice_grid = {}
longest_entrance = {}
for nexus in choice_spots:
    this_toward = []
    for nb in choice_spots[nexus]:
        vis = set([nexus, nb])
        while nb not in choice_spots and 0 < nb[0] < height - 1:
            (x, y) = nb
            new_nbs = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]
            good_nbs = [n for n in new_nbs if grid_d[n] != "#" and n not in vis]
            assert len(good_nbs) == 1, f"At {nb}, visited is {vis}, good is {good_nbs}"
            nb = good_nbs[0]
            vis.add(nb)
        if nb[0] == 0 or nb[0] == height - 1:
            choice_grid[nb] = [(nexus, len(vis) - 1)]
            longest_entrance[nb] = len(vis) - 1
        this_toward.append((nb, len(vis) - 1))
    longest_entrance[nexus] = max(dist for (_, dist) in this_toward)
    # only allow the single "out" from the node that's the unique node that
    # can reach the exit
    if any(nb[0] == height - 1 for (nb, _) in this_toward):
        this_toward = [(nb, dist) for (nb, dist) in this_toward if nb[0] == height - 1]
    choice_grid[nexus] = this_toward


def p2() -> int:
    max_potential = sum(longest_entrance.values()) - longest_entrance[(0, 1)]
    print("max_potential", max_potential)
    pos_map = dict((v, idx) for (idx, v) in enumerate(sorted(choice_grid)))
    longest_entrance_ = dict((pos_map[k], v) for (k, v) in longest_entrance.items())
    choice_grid_ = dict(
        (pos_map[k], [(pos_map[nb], dist) for (nb, dist) in v]) for (k, v) in choice_grid.items()
    )
    end = max(pos_map.values())
    workq = [(0, max_potential, 0, 0)]
    maxlen = 0
    while workq:
        (neg_sofar, pot, pos, visited) = heapq.heappop(workq)
        if maxlen >= pot:
            # can already do better than best of this
            continue
        if pos == end:
            maxlen0 = maxlen
            maxlen = max(maxlen, -neg_sofar)
            if maxlen0 != maxlen:
                print("DBG", maxlen)
        nvisited = visited | (1 << pos)
        for dest_pos, dest_d in choice_grid_[pos]:
            if (1 << dest_pos) & visited == 0:
                pot_lost = longest_entrance_[dest_pos] - dest_d
                heapq.heappush(workq, (neg_sofar - dest_d, pot - pot_lost, dest_pos, nvisited))
    return maxlen


# print(json.dumps({str(k):v for (k, v) in choice_grid.items()}, sort_keys=True))

print(p2())
