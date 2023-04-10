# AOC 2018 day 23 solution to defeat adversarial input (python3)
# Adversarial input sample can be found https://pastebin.com/raw/9eJQN836
# This will try to open the file given as the first command line argument
# or "aoc23.in.txt" if no argument is given.

# This solution transforms the given coordinates in x-y-z space into 4D
# coordinates in a space I call s-t-u-v space, even though I never actually
# deal with 's', 't', 'u', or 'v' directly.

import itertools
import numpy as np
import sys
import re
import heapq


# Global setup for interesting_points, below

# The idea is that I'm creating matrices that can later find me the
# intersection in x-y-z space of three planes drawn from the collection of
# eleven planes given by the eight planes that define a given box in s-t-u-v
# space and the three planes x=0, y=0, and z=0. The closest point to the
# origin within the box must be at such an intersection.

choose_src = np.array(
    [[-1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0],
     [1, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0],
     [1, 1, -1, 0, 0, 1, 0, 0, 0, 0, 0],
     [1, 1, 1,  0, 0, 0, 1, 0, 0, 0, 0],
     [-1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0],
     [1, -1, 1, 0, 0, 0, 0, 0, 1, 0, 0],
     [1, 1, -1, 0, 0, 0, 0, 0, 0, 1, 0],
     [1, 1, 1,  0, 0, 0, 0, 0, 0, 0, 1],
     [1, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0],
     [0, 1, 0,  0, 0, 0, 0, 0, 0, 0, 0],
     [0, 0, 1,  0, 0, 0, 0, 0, 0, 0, 0]]
    )

# Note that choose_src is 11 x 11.

# I'm going to take three rows at a time from choose_src, then split
# the result into a 3x3 matrix on the left, and a 3x8 matrix on the
# right. (call them L and R) I'm then later going to want to find
# the vector xyz such that:
#     L @ xyz == R @ box_bounds_stuv
# where box_bounds_stuv is the eight numbers that define the 4D box in
# s-t-u-v space: [min_x, min_t, min_u, min_v, max_s, max_t, max_u, max_v]
#
# to do that easily later, here I remember the matrix
#     matr = inv(L) @ R

interesting_matrices = []
for rows in itertools.combinations(range(11), 3):
    sub = choose_src[rows, :]
    if np.linalg.det(sub[:3, :3]) == 0.0:
        continue
    matr = np.linalg.inv(sub[:3, :3]) @ sub[:3, 3:]
    if all((matr != m).any() for m in interesting_matrices):
        # we haven't seen matr before
        # yeah, repeated linear scan over interesting_matrices, but we
        # have fewer than 165 (11 choose 3, but some have det == 0.0)
        # possibilities, so it's not a big deal
        interesting_matrices.append(matr)

interesting_matrices = np.array(interesting_matrices)


def which_bots(box0, box1):
    box4_within = ((box0 <= bot_max) & (box1 > bot_min))
    return box4_within.all(axis=1)


def interesting_points(box0, box1):
    """
    Return points inside the four-dimensional box that might have the
    smallest distance-to-origin. That'll be any point that corresponds
    to a corner of the box or to the intersection of one or more
    bounding planes of the box and one of the x=0, y=0, or z=0 planes.

    To guard against possible off-by-one errors introduced by float->int
    conversion, we perturb each xyz coordinate by up to one in each
    direction before converting back to stuv-coordinates and checking
    membership in the box.
    """
    box_coords = (np.array([box0, box1]) - [[0], [1]]).reshape((8, 1))
    interesting_xyz = (interesting_matrices @ box_coords).astype(int)
    interesting_xyz = interesting_xyz.reshape(
        interesting_xyz.shape[0], 1, 3, 1)

    add_array = np.array(list(itertools.product(
        (-1, 0, 1), (-1, 0, 1), (-1, 0, 1)))).reshape(27, 3, 1)

    all_xyz = (add_array + interesting_xyz).reshape(
        27*(interesting_xyz.shape[0]), 3, 1)
    all_stuv = np.array([[-1, 1, 1],
                         [1, -1, 1],
                         [1, 1, -1],
                         [1, 1, 1]]) @ all_xyz
    all_stuv = all_stuv.reshape(all_stuv.shape[0], 4)
    valid = ((np.array(box0) <= all_stuv) &
             (all_stuv < np.array(box1))).all(axis=1)
    return sorted(set(tuple(x) for x in all_stuv[valid]))


def min_distance_to_origin(box0, box1):
    interesting = interesting_points(box0, box1)
    if not interesting:
        # Can happen if box is such that there are no
        # points in it with s % 2 == t % 2 == u % 2
        # and v = s + t + u
        return None
    else:
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

    bot_coords = np.array([[-1, 1, 1],
                           [1, -1, 1],
                           [1, 1, -1],
                           [1, 1, 1]]) @ (
                               np.array([b[0:3] for b in bots]).transpose())
    bot_coords = bot_coords.transpose()
    bot_radii = np.array([b[3] for b in bots]).reshape((len(bots), 1))
    bot_max = bot_coords + bot_radii
    bot_min = bot_coords - bot_radii
    #print(bot_coords.shape)
    #print(bot_radii.shape)

    maxradbot_max = bot_max[maxradbot, :]
    maxradbot_min = bot_min[maxradbot, :]

    print("bots in range of maxrad bot",
          (
              (bot_coords >= maxradbot_min) & (bot_coords <= maxradbot_max)
          ).all(axis=1).astype(int).sum())

    # Find a box big enough to contain everything in range
    maxabscord = max(abs(bot_max).max(), abs(bot_min).max()) + maxrad

    workheap = []
    corner0 = tuple([-maxabscord] * 4)
    corner1 = tuple([maxabscord] * 4)
    workheap.append(
        (-len(bots), 0, 16*(maxabscord**4), (corner0, corner1)))

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

    print_bots = False  # Change this to see the bots in range
    i = 0
    while workheap:
        i += 1
        (negreach, dist_to_orig, sz, box) = heapq.heappop(workheap)
        box0 = np.array(box[0])
        box1 = np.array(box[1])
        if sz == 1:
            print("Found closest at %s dist %s (%s bots in range)" %
                  (str(box[0]), dist_to_orig, -negreach))
            print("Normal coords",
                  ((box[0][0]+box[0][1])//2,
                   (box[0][2]+box[0][1])//2,
                   (box[0][0]+box[0][2])//2))
            if print_bots:
                print("bots:")
                mine = which_bots(box[0], box[1])
                for c in np.flatnonzero(mine):
                    print(c+1, ':', data[c].strip(), bot_min[c], bot_max[c])
            break
        # Debugging/tuning:
        print(-negreach, sz, dist_to_orig, i, len(workheap), end='')
        mybots = which_bots(box0, box1)
        subboxes = []
        for axis in (0, 1, 2, 3):
            arange = (box0[axis], box1[axis])
            division_points = (set(arange) | set(bot_min[mybots, axis])
                               | set(bot_max[mybots, axis] + 1))
            division_points = list(x for x in division_points if x >= arange[0]
                                   and x <= arange[1])
            if len(division_points) >= 3:
                division_points.sort()
                for (lo, hi) in ((division_points[ndx], division_points[ndx+1])
                                 for ndx in range(len(division_points)-1)):
                    newbox0 = list(box[0])
                    newbox1 = list(box[1])
                    newbox0[axis] = lo
                    newbox1[axis] = hi
                    subboxes.append((tuple(newbox0), tuple(newbox1)))
                print(f' (axis {axis})')
                break
        else:
            # Everything is now in the same bots' ranges. Therefore only the
            # interesting points matter:
            ip = interesting_points(box0, box1)
            if ip:
                print(f' (interesting points: {box})')
                for pt in ip:
                    subboxes.append((pt, tuple(np.array(pt) + 1)))
            else:
                print(' (backup halving)')
                # halve in each dimension
                halfway = tuple((box[0][i] + box[1][i]) // 2
                                for i in (0, 1, 2, 3))
                for octant in itertools.product((0, 1), (0, 1), (0, 1), (0, 1)):
                    newbox0 = tuple(halfway[i] if octant[i] else box[0][i]
                                    for i in (0, 1, 2, 3))
                    newbox1 = tuple(box[1][i] if octant[i] else halfway[i]
                                    for i in (0, 1, 2, 3))
                    subboxes.append((newbox0, newbox1))

        for (newbox0, newbox1) in subboxes:
            if newbox0[3] < sum(newbox0[0:3]):
                newbox0 = newbox0[0:3] + (sum(newbox0[0:3]),)
            if newbox1[3] > sum(newbox1[0:3]):
                newbox1 = newbox1[0:3] + (sum(newbox1[0:3]),)
            sz_box = (int(newbox1[0] - newbox0[0]),
                      int(newbox1[1] - newbox0[1]),
                      int(newbox1[2] - newbox0[2]))
            newsz = (sz_box[0] * sz_box[1] * sz_box[2])
            if newsz <= 0:
                continue
            newreach = which_bots(newbox0, newbox1).astype(int).sum()
            if newreach > 0:
                d_t_o = min_distance_to_origin(newbox0, newbox1)
                if d_t_o is not None:
                    heapq.heappush(workheap, (-newreach, d_t_o, newsz,
                                              (newbox0, newbox1)))
