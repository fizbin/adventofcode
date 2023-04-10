from __future__ import print_function

import sys
import re
import collections
import numpy as np

with open('aoc20.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read().strip()

if data[0] != '^':
    raise Exception("Didn't start with ^")
if data[-1] != '$':
    raise Exception("Didn't end with $")

global_directions = data[1:-1]

offset = (511, 511)

def findmatching(curr_indx, target, directions):
    i = curr_indx + 1
    while i < len(directions):
        now = directions[i]
        if now == target:
            return i
        if now == '(':
            i = findmatching(i, ')', directions) + 1
        elif now == ')':
            return None
        else:
            i += 1
    else:
        raise Exception("findmatching(%r, %r)" % (curr_indx, target))


def move(loc, act):
    if act == 'N':
        return (loc[0], loc[1] + 1)
    if act == 'S':
        return (loc[0], loc[1] - 1)
    if act == 'E':
        return (loc[0] + 1, loc[1])
    if act == 'W':
        return (loc[0] - 1, loc[1])
    raise Exception("move(%r, %r)" % (loc, act))


def handle(directions):
    print("Handling %s" % directions)
    # returns ((10000,10000,4) dtype=bool, [endspots]) with offset as startpoint
    if not directions:
        doors = np.full((offset[0]*2, offset[1]*2, 4), False, dtype=bool)
        loc = offset
        return (doors, [loc])
    m = re.match('^[NSEW]+', directions)
    if m:
        doors = np.full((offset[0]*2, offset[1]*2, 4), False, dtype=bool)
        loc = offset
        for act in m.group(0):
            nxt = move(loc, act)
            doors[loc[0], loc[1], 'NSEW'.index(act)] = True
            doors[nxt[0], nxt[1], 1 ^ ('NSEW'.index(act))] = True
            loc = nxt
        (remainderdoors, ends) = handle(directions[len(m.group(0)):])
        roll = (loc[0] - offset[0], loc[1] - offset[1])
        remainderdoors_adj = np.roll(remainderdoors, roll, axis=(0, 1))
        if roll[0] > 0:
            remainderdoors_adj[:roll[0], :] = False
        elif roll[0] < 0:
            remainderdoors_adj[roll[0]:, :] = False
        if roll[1] > 0:
            remainderdoors_adj[:, :roll[1]] = False
        if roll[1] < 0:
            remainderdoors_adj[:, roll[1]:] = False
        remainderdoors_adj |= doors
        return (remainderdoors_adj, [(e[0] + roll[0], e[1] + loc[1])
                                     for e in ends])
    if directions[0] == '(':
        if findmatching(0, ')', directions) == len(directions) - 1:
            subs = []
            i = 0
            nextbar = findmatching(i, '|', directions)
            while nextbar is not None:
                subs.append(directions[i+1:nextbar])
                i = nextbar
                nextbar = findmatching(i, '|', directions)
            subs.append(directions[i+1:-1])
            ends = []
            doors = np.full((offset[0]*2, offset[1]*2, 4), False, dtype=bool)
            for sub in subs:
                (d1, e1) = handle(sub)
                doors |= d1
                ends.extend(e1)
            return (doors, sorted(set(ends)))
        else:
            match = findmatching(0, ')', directions)
            (left_doors, left_ends) = handle(directions[:match + 1])
            (right_doors, right_ends) = handle(directions[match + 1:])
            ends = []
            for left in left_ends:
                roll = (left[0] - offset[0], left[1] - offset[1])
                right_doors_adj = np.roll(right_doors, roll, axis=(0, 1))
                if roll[0] > 0:
                    right_doors_adj[:roll[0], :] = False
                elif roll[0] < 0:
                    right_doors_adj[roll[0]:, :] = False
                if roll[1] > 0:
                    right_doors_adj[:, :roll[1]] = False
                if roll[1] < 0:
                    right_doors_adj[:, roll[1]:] = False
                left_doors |= right_doors
                ends.extend((e[0] + left[0], e[1] + left[1])
                            for e in right_ends)
            return (left_doors, sorted(set(ends)))
    raise Exception("Starting with %r" % (directions[0],))


(doors, _) = handle(global_directions)
dists = {offset: 0}
workstack = collections.deque([offset])
while workstack:
    loc = workstack.pop()
    for (n, act) in enumerate('NSEW'):
        if doors[loc[0], loc[1], n]:
            nxt = move(loc, act)
            pdist = dists.get(nxt)
            if pdist is None or pdist > dists[loc] + 1:
                workstack.append(nxt)
                dists[nxt] = dists[loc] + 1

#for i in range(4):
#    print(doors[:,:,i].astype(int))
print(max(dists.values()))

print(len([x for (x, d) in dists.items() if d >= 1000]))
# W(N|S)E
# ((0, 0), 0) []
#    W
# ((-1, 0), 1) []
#    (
# ((-1, 0), 2) [((-1, 0), 2)]
#    N
# ((-1, 1), 3) [((-1, 0), 2)]
#    |
#
