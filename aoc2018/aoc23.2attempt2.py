from __future__ import print_function

import itertools
import numpy as np
import sys
import re
import heapq


def d3(a, b):
    return abs(a[0]-b[0])+abs(a[1]-b[1])+abs(a[2]-b[2])


def does_intersect(box, bot):
    # returns whether box intersects bot
    d = 0
    for i in (0, 1, 2):
        boxlow, boxhigh = box[0][i], box[1][i] - 1
        d += abs(bot[i] - boxlow) + abs(bot[i] - boxhigh)
        d -= boxhigh - boxlow
    d //= 2
    return d <= bot[3]


def intersect_count(box):
    return sum(1 for b in bots if does_intersect(box, b))


matr_2octa1cube = []
matr_1octa2cube = []
octaface_directions = [(1, -1, -1), (1, -1, 1), (1, 1, -1), (1, 1, 1)]
cubeface_directions = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]
for (octaface1, octaface2) in itertools.combinations(octaface_directions, 2):
    for cubeface in cubeface_directions:
        matr = np.array((octaface1, octaface2, cubeface))
        imatr = np.linalg.inv(matr)
        matr_2octa1cube.append((matr, imatr))
for octaface in octaface_directions:
    for (cubeface1, cubeface2) in itertools.combinations(cubeface_directions, 2):
        matr = np.array((octaface, cubeface1, cubeface2))
        imatr = np.linalg.inv(matr)
        matr_1octa2cube.append((matr, imatr))


def find_intersections(box, bot):
    """Find all endpoints of intersection"""
    def is_in_box(pt):
        return ((pt[0] >= box[0][0]) and (pt[0] < box[1][0]) and
                (pt[1] >= box[0][1]) and (pt[1] < box[1][1]) and
                (pt[2] >= box[0][2]) and (pt[2] < box[1][2]))

    def is_in_bot(pt):
        return (sum(abs(pt[i] - bot[i]) for i in (0, 1, 2)) <= bot[3])

    cubecorners = list(
        itertools.product(*((box[0][i], box[1][i]-1) for i in (0, 1, 2))))
    botcorners = [
        (bot[0] + bot[3], bot[1], bot[2]), (bot[0] - bot[3], bot[1], bot[2]),
        (bot[0], bot[1] + bot[3], bot[2]), (bot[0], bot[1] - bot[3], bot[2]),
        (bot[0], bot[1], bot[2] + bot[3]), (bot[0], bot[1], bot[2] - bot[3])]
    candidates = cubecorners + botcorners
    ptmatr = np.array(
        (cubecorners[0], cubecorners[-1], botcorners[0], botcorners[1])).transpose()
    for (matr, imatr) in matr_2octa1cube:
        g = matr @ ptmatr
        pts = imatr @ np.transpose([[g[0][o1], g[1][o2], g[2][c1]]
                                    for o1 in (0, 1) for o2 in (0, 1)
                                    for c1 in (2, 3)])
        candidates += [tuple(pts[:, i].astype(int)) for i in range(8)]
    for (matr, imatr) in matr_1octa2cube:
        g = matr @ ptmatr
        pts = imatr @ np.transpose([[g[0][o1], g[1][c1], g[2][c2]]
                                    for o1 in (0, 1) for c1 in (2, 3)
                                    for c2 in (2, 3)])
        candidates += [tuple(pts[:, i].astype(int)) for i in range(8)]
    return [c for c in candidates if is_in_box(c) and is_in_bot(c)]


if __name__ == '__main__':
    with open('aoc23.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(f)

    bots = [tuple(map(int, list(re.findall(r'-?\d+', ln)))) for ln in data]

    maxrad = max(r for (x, y, z, r) in bots)
    maxradbot = [b for b in bots if b[3] == maxrad][0]

    print("bots in range of maxrad bot",
          sum(1 for b in bots if d3(b, maxradbot) <= maxrad))

    # Find a box big enough to contain everything in range
    maxabscord = max(max(abs(b[i])+b[3] for b in bots) for i in (0, 1, 2))
    boxsize = 1
    while boxsize <= maxabscord:
        boxsize *= 2

    initial_box = ((-boxsize, -boxsize, -boxsize), (boxsize, boxsize, boxsize))

    # Set up heap to work on things first by number of bots in range of box,
    # then by size of box, then by distance to origin
    #
    # The idea is that we first work on a box with the most bots in range.
    # In the event of a tie, work on the larger box.
    # In the event of a tie, work on the one with a min corner closest
    # to the origin.
    #
    # These rules mean that if I get to where I'm processing a 1x1x1 box,
    # I know I'm done:
    # - no larger box can intersect as many bots' ranges as what I'm working on
    # - no other 1x1x1 box intersecting the same number of bots can be as close

    # remember heapq.heappop pulls the smallest off the heap, so negate
    # the two things I want to pull by largest (reach of box, boxsize) and
    # do not negate distance-to-origin, since I want to work on smallest
    # distance-to-origin first

    workheap = [(-len(bots), 0, 2*boxsize, initial_box)]
    max_for_size = {(2*boxsize): len(bots)}
    while workheap:
        (negreach, dist_to_orig, sz, box) = heapq.heappop(workheap)
        if sz == 1:
            print("Found closest at %s dist %s (%s bots in range)" %
                  (str(box[0]), dist_to_orig, -negreach))
            break
        print(sz, dist_to_orig)
        newsz = sz // 2
        for octant in [(0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1),
                       (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1)]:
            newbox0 = tuple(box[0][i] + newsz * octant[i] for i in (0, 1, 2))
            newbox1 = tuple(newbox0[i] + newsz for i in (0, 1, 2))
            newbox = (newbox0, newbox1)
            newreach = intersect_count(newbox)
            closest_to_origin = (newbox0[0] + (sz - 1)*int(newbox0[0] < 0),
                                 newbox0[1] + (sz - 1)*int(newbox0[1] < 0),
                                 newbox0[2] + (sz - 1)*int(newbox0[2] < 0))
            heapq.heappush(workheap,
                           (-newreach, d3(closest_to_origin, (0, 0, 0)),
                            newsz, newbox))
