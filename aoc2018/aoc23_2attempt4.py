from __future__ import print_function

import itertools
import numpy as np
import sys
import re
import heapq
from functools import total_ordering

def interesting_points(box0, box1):
    box_corners = list(itertools.product((box0[0], box1[0] - 1),
                                         (box0[1], box1[1] - 1),
                                         (box0[2], box1[2] - 1)))
    xyz_corners = [((q + r) // 2, (r + s) // 2, (q + s) // 2)
                   for (q, r, s) in box_corners]
    interesting_xyz = list(xyz_corners)
    straddles_x = any((c1[0] >= 0) != (c2[0] >= 0)
                      for (c1, c2) in itertools.combinations(xyz_corners, 2))
    straddles_y = any((c1[1] >= 0) != (c2[1] >= 0)
                      for (c1, c2) in itertools.combinations(xyz_corners, 2))
    straddles_z = any((c1[2] >= 0) != (c2[2] >= 0)
                      for (c1, c2) in itertools.combinations(xyz_corners, 2))
    if straddles_x:
        # need to add intersections with x == 0
        interesting_xyz.extend(
            (0, (-boxA[0] + boxB[2]) // 2, (boxA[0] + boxB[2]) // 2)
            for (boxA, boxB) in itertools.product((box0, box1), (box0, box1)))
        interesting_xyz.extend(
            (0, (boxA[1] + boxB[2]) // 2, (-boxA[1] + boxB[2]) // 2)
            for (boxA, boxB) in itertools.product((box0, box1), (box0, box1)))
        if straddles_y:
            interesting_xyz.extend((0, 0, box[0]) for box in (box0, box1))
            interesting_xyz.extend((0, 0, -box[1]) for box in (box0, box1))
            interesting_xyz.extend((0, 0, box[2]) for box in (box0, box1))
            if straddles_z:
                interesting_xyz.append((0, 0, 0))
        elif straddles_z:
            interesting_xyz.extend((0, -box[0], 0) for box in (box0, box1))
            interesting_xyz.extend((0, box[1], 0) for box in (box0, box1))
            interesting_xyz.extend((0, box[2], 0) for box in (box0, box1))
    elif straddles_y:
        interesting_xyz.extend(
            ((boxA[0] + boxB[1]) // 2, 0, (boxA[0] - boxB[1]) // 2)
            for (boxA, boxB) in itertools.product((box0, box1), (box0, box1)))
        interesting_xyz.extend(
            ((boxA[0] - boxB[2]) // 2, 0, (boxA[0] + boxB[2]) // 2)
            for (boxA, boxB) in itertools.product((box0, box1), (box0, box1)))
        if straddles_z:
            interesting_xyz.extend((box[0], 0, 0) for box in (box0, box1))
            interesting_xyz.extend((box[1], 0, 0) for box in (box0, box1))
            interesting_xyz.extend((-box[2], 0, 0) for box in (box0, box1))
    elif straddles_z:
        interesting_xyz.extend(
            ((boxA[0] + boxB[1]) // 2, (-boxA[0] + boxB[1]) // 2, 0)
            for (boxA, boxB) in itertools.product((box0, box1), (box0, box1)))
        interesting_xyz.extend(
            ((boxA[1] - boxB[2]) // 2, (boxA[1] + boxB[2]) // 2, 0)
            for (boxA, boxB) in itertools.product((box0, box1), (box0, box1)))
    interesting = []
    for (x, y, z) in interesting_xyz:
        for (xa, ya, za) in itertools.product((-1, 0, 1), (-1, 0, 1), (-1, 0, 1)):
            (xp, yp, zp) = (x + xa, y + ya, z + za)
            (p, q, r) = (xp - yp + zp, xp + yp - zp, -xp + yp + zp)
            if (box0[0] <= p and p < box1[0] and
                box0[1] <= q and q < box1[1] and
                    box0[2] <= r and r < box1[2]):
                interesting.append((p, q, r))
    return sorted(set(interesting))


def min_distance_to_origin(box0, box1):
    interesting = interesting_points(box0, box1)
    return min(abs(c[0]+c[1])//2 +
               abs(c[1]+c[2])//2 +
               abs(c[0]+c[2])//2
               for c in interesting)


if __name__ == '__main__':
    with open('aoc23.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(f)

    bots = [tuple(map(int, list(re.findall(r'-?\d+', ln)))) for ln in data]

    maxrad = max(r for (x, y, z, r) in bots)
    maxradbot = [n for (n, b) in enumerate(bots) if b[3] == maxrad][0]

    bot_coords = np.array([[1, -1, 1],
                           [1, 1, -1],
                           [-1, 1, 1],
                           [1, 1, 1]]) @ (
                               np.array([b[0:3] for b in bots]).transpose())
    bot_coords = bot_coords.transpose()
    bot_radii = np.array([b[3] for b in bots]).reshape((len(bots), 1))
    bot_max = bot_coords + bot_radii
    bot_min = bot_coords - bot_radii
    print(bot_coords.shape)
    print(bot_radii.shape)

    maxradbot_max = bot_max[maxradbot, :]
    maxradbot_min = bot_min[maxradbot, :]

    print("bots in range of maxrad bot",
          (
              (bot_coords >= maxradbot_min) & (bot_coords <= maxradbot_max)
          ).all(axis=1).astype(int).sum())

    # Find a box big enough to contain everything in range
    maxabscord = max(abs(bot_max).max(), abs(bot_min).max())
    boxsize = 1
    while boxsize <= maxabscord:
        boxsize *= 2

    print(boxsize)
    initial_box = ((-boxsize, -boxsize, -boxsize), (boxsize, boxsize, boxsize))

    # Set up heap to work on boxes
    #
    # The idea is that we first work on a box with the most bots in range.
    # In the event of a tie, work on the one with the lowest estimate for
    # "distance to origin of intersection". In case that still ties, use
    # the smallest box.
    #
    # These rules mean that if I get to where I'm processing a 1x1x1 box,
    # I know I'm done:
    # - no larger box can intersect more bots' ranges than what I'm working on
    # - no other box intersecting the same number of bots can be as close
    #
    # Getting this right depends crucially on the fact that the estimate
    # of distance can never overestimate, only underestimate, and also that
    # it's guaranteed accurate for a 1x1x1 box.

    # remember heapq.heappop pulls the smallest off the heap, so negate
    # the two things I want to pull by largest (reach of box, boxsize) and
    # do not negate distance-to-origin, since I want to work on smallest
    # distance-to-origin first

    workheap = [(-len(bots), 0, -2*boxsize, initial_box)]
    i = 0
    while workheap:
        i += 1
        (negreach, dist_to_orig, negsz, box) = heapq.heappop(workheap)
        sz = -negsz
        if sz == 1:
            print("Found closest at %s dist %s (%s bots in range)" %
                  (str(box[0]), dist_to_orig, -negreach))
            break
        # Debugging/tuning:
        print(-negreach, sz, dist_to_orig, i, len(workheap))
        newsz = sz // 2
        for octant in [(0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1),
                       (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1)]:
            newbox0 = tuple(box[0][i] + newsz * octant[i] for i in (0, 1, 2))
            newbox1 = tuple(newbox0[i] + newsz for i in (0, 1, 2))
            box4_min = np.array(newbox0 + (sum(newbox0),))
            box4_max = np.array(newbox1 + (sum(newbox1),))
            newbox = (newbox0, newbox1)
            box4_within = ((box4_min <= bot_max) & (box4_max > bot_min))
            newreach = box4_within.all(axis=1).astype(int).sum()
            print(newbox, 'as', (box4_min, box4_max), '=>', newreach)
            if newreach > 0:
                if ((newbox0[0] % 2) != (newbox0[1] % 2) or
                        (newbox0[0] % 2) != (newbox0[2] % 2)):
                    continue
                d_t_o = min_distance_to_origin(newbox0, newbox1)
                heapq.heappush(workheap, (-newreach, d_t_o, -newsz, newbox))
