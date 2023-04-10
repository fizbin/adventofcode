from __future__ import print_function

import itertools
import numpy as np
import sys
import re
import heapq
from functools import total_ordering


@total_ordering
class LazyInt(object):
    def __init__(self, iproc):
        self.iproc = iproc
        self.ival = None

    def __int__(self):
        if self.ival is None:
            self.ival = self.iproc()
        return self.ival

    def __eq__(self, other):
        return int(self) == int(other)

    def __lt__(self, other):
        return int(self) < int(other)

    def __str__(self):
        return str(int(self))

    def __repr__(self):
        if self.ival is None:
            return f"LazyInt({self.iproc})"
        return f"LazyInt(lambda: {self.ival})"


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


# setup for find_intersections, below
matr_2octa1cube = []
matr_1octa2cube = []
octaface_directions = [(1, -1, -1), (1, -1, 1), (1, 1, -1), (1, 1, 1)]
cubeface_directions = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]
for (octaface1, octaface2) in itertools.combinations(octaface_directions, 2):
    for cubeface in cubeface_directions:
        matr = np.array((octaface1, octaface2, cubeface))
        if np.linalg.det(matr) == 0.0:
            continue
        imatr = np.linalg.inv(matr)
        matr_2octa1cube.append((matr, imatr))
for octaface in octaface_directions:
    for (cubeface1, cubeface2) in itertools.combinations(cubeface_directions, 2):
        matr = np.array((octaface, cubeface1, cubeface2))
        imatr = np.linalg.inv(matr)
        matr_1octa2cube.append((matr, imatr))


def find_intersections(box, bot):
    """
    Find all intersections of three of the defining planes of the box and the
    bot's octahedron that are within the box and the bot's octahedron.

    Any linear function over coordinates (such as Manhattan distance to origin)
    will take on its extreme (max and min) values over the whole intersection
    at one of these points.

    (Yes, Manhattan distance to origin is a linear function here because after
    the initial big box, every box is entirely within a single octant)
    """
    def is_in_box(pt):
        return ((pt[0] >= box[0][0]) and (pt[0] < box[1][0]) and
                (pt[1] >= box[0][1]) and (pt[1] < box[1][1]) and
                (pt[2] >= box[0][2]) and (pt[2] < box[1][2]))

    def is_in_bot(pt):
        return (sum(abs(pt[i] - bot[i]) for i in (0, 1, 2)) <= bot[3])

    # Three cube planes
    cubecorners = list(
        itertools.product(*((box[0][i], box[1][i]-1) for i in (0, 1, 2))))
    if all(is_in_bot(p) for p in cubecorners):
        return cubecorners

    # Three bot octahedron planes
    botcorners = [
        (bot[0] + bot[3], bot[1], bot[2]), (bot[0] - bot[3], bot[1], bot[2]),
        (bot[0], bot[1] + bot[3], bot[2]), (bot[0], bot[1] - bot[3], bot[2]),
        (bot[0], bot[1], bot[2] + bot[3]), (bot[0], bot[1], bot[2] - bot[3])]
    if all(is_in_box(p) for p in botcorners):
        return botcorners

    # Tricky case: 2 planes of one, 1 plane of the other
    ptmatr = np.array(
        (botcorners[0], botcorners[1], cubecorners[0], cubecorners[-1])
    ).transpose()
    intersect1 = []
    intersect2 = []
    for (matr, imatr) in matr_2octa1cube:
        g = matr @ ptmatr
        pts = imatr @ np.transpose([[g[0][o1], g[1][o2], g[2][c1]]
                                    for o1 in (0, 1) for o2 in (0, 1)
                                    for c1 in (2, 3)])
        intersect1 += [tuple(pts[:, i].astype(int)) for i in range(8)]
    for (matr, imatr) in matr_1octa2cube:
        g = matr @ ptmatr
        pts = imatr @ np.transpose([[g[0][o1], g[1][c1], g[2][c2]]
                                    for o1 in (0, 1) for c1 in (2, 3)
                                    for c2 in (2, 3)])
        intersect2 += [tuple(pts[:, i].astype(int)) for i in range(8)]
    candidates = cubecorners + botcorners + intersect1 + intersect2
    return sorted(set([c for c in candidates if is_in_box(c) and is_in_bot(c)]))


def find_closest_orig_point(box, check_bots):
    """
    Generate an estimate of how close we'll be to the origin if we end up
    at the intersection of all bot ranges in check_bots that happens in this
    box. Do this by finding for each bot the closest that the intersection of
    the box and the bot's range gets to the origin, and then find the maximum
    of those values.

    Note that is can be an underestimate of the correct value (counting cases
    when the correct value may not exist at all because all bots in check_bots
    don't intersect as an underestimate), but can't be an overestimate, since
    we're guaranteed to have the point in the box and bot's range that's
    closest to the origin somewhere in the "intersections" list, and any
    intersection of all the bots' ranges and the box must be at least as far
    from the origin as that.

    Note also that if the box is 1x1x1, the estimate is guaranteed to be
    exactly correct.
    """
    maxmindist = 0
    for b in check_bots:
        intersections = find_intersections(box, b)
        orig_dist = min(sum(abs(x) for x in isct)
                        for isct in intersections)
        maxmindist = max(orig_dist, maxmindist)
    return int(maxmindist)


def intersect_count(box):
    intersect_bots = []
    for b in bots:
        if does_intersect(box, b):
            intersect_bots.append(b)

    if not intersect_bots:
        return (0, LazyInt(lambda: 999999999))
    bot_array = np.array(intersect_bots)
    bot_coords = bot_array[:, :3]
    bot_radii = bot_array[:, 3]
    cross_distance = abs(np.fromfunction(
        lambda x, y: bot_coords[x] - bot_coords[y],
        (len(bot_array), len(bot_array)), dtype=int)).sum(axis=2)
    cross_radii = np.fromfunction(
        lambda x, y: bot_radii[x] + bot_radii[y],
        cross_distance.shape, dtype=int)
    max_inters = (cross_distance <= cross_radii).astype(int).sum(axis=1).max()
    # Check too many, each step is slow. Check too few, and too many steps.
    # (especially when given small numbers of bots)
    check_bots = intersect_bots[:50]
    return (max_inters,
            LazyInt(lambda: find_closest_orig_point(box, check_bots)))


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

    workheap = [(-len(bots), LazyInt(lambda: 0), 2*boxsize, initial_box)]
    max_for_size = {(2*boxsize): len(bots)}
    while workheap:
        (negreach, dist_to_orig, sz, box) = heapq.heappop(workheap)
        if sz == 1:
            print("Found closest at %s dist %s (%s bots in range)" %
                  (str(box[0]), dist_to_orig, -negreach))
            break
        # Debugging/tuning:
        print(-negreach, sz, dist_to_orig)
        newsz = sz // 2
        for octant in [(0, 0, 0), (0, 0, 1), (0, 1, 0), (0, 1, 1),
                       (1, 0, 0), (1, 0, 1), (1, 1, 0), (1, 1, 1)]:
            newbox0 = tuple(box[0][i] + newsz * octant[i] for i in (0, 1, 2))
            newbox1 = tuple(newbox0[i] + newsz for i in (0, 1, 2))
            newbox = (newbox0, newbox1)
            (newreach, mindist) = intersect_count(newbox)

            if newreach > 0:
                heapq.heappush(workheap,
                               (-newreach, mindist, newsz, newbox))
