#!/usr/bin/python

import aoc_util
import copy
import numpy as np

data = aoc_util.get_data_lines(22)

max_x, max_y, max_z = 0, 0, 0
iblocks = []
for line in data:
    iblocks.append(tuple(aoc_util.numbers(line)))
    max_x = max(max_x, iblocks[-1][0], iblocks[-1][3])
    max_y = max(max_y, iblocks[-1][1], iblocks[-1][4])
    max_z = max(max_z, iblocks[-1][2], iblocks[-1][5])


def do_dropping(blocks0):
    blocks = copy.deepcopy(blocks0)
    keep_going = True
    while keep_going:
        keep_going = False
        space = np.zeros((max_x + 1, max_y + 1, max_z + 1), dtype=int)
        for blk in blocks:
            (x1, y1, z1, x2, y2, z2) = blk
            space[range(x1, x2 + 1), range(y1, y2 + 1), range(z1, z2 + 1)] = 1
        for idx, blk in list(enumerate(blocks)):
            (x1, y1, z1, x2, y2, z2) = blk
            if z1 == 1:
                continue
            drop = 0
            if z1 != z2:
                while (
                    z1 - drop > 1
                    and not space[range(x1, x2 + 1), range(y1, y2 + 1), z1 - drop - 1].any()
                ):
                    drop += 1
            else:
                while (
                    z1 - drop > 1
                    and not space[
                        range(x1, x2 + 1),
                        range(y1, y2 + 1),
                        range(z1 - drop - 1, z2 - drop - 1 + 1),
                    ].any()
                ):
                    drop += 1
            if drop > 0:
                keep_going = True
                blocks[idx] = (x1, y1, z1 - drop, x2, y2, z2 - drop)
                space[range(x1, x2 + 1), range(y1, y2 + 1), range(z1, z2 + 1)] = 0
                space[range(x1, x2 + 1), range(y1, y2 + 1), range(z1 - drop, z2 + 1 - drop)] = 1
    return (blocks, len([idx for idx in range(len(blocks)) if blocks[idx] != blocks0[idx]]))


blocks, _ = do_dropping(iblocks)
droppable = set(range(len(blocks)))
support = {}
space = np.zeros((max_x + 1, max_y + 1, max_z + 1), dtype=int) - 1
for idx, blk in enumerate(blocks):
    (x1, y1, z1, x2, y2, z2) = blk
    space[range(x1, x2 + 1), range(y1, y2 + 1), range(z1, z2 + 1)] = idx
for idx, blk in enumerate(blocks):
    (x1, y1, z1, x2, y2, z2) = blk
    if z1 == 1:
        support[idx] = set([-1])
        continue
    if z1 != z2:
        onebelow = space[range(x1, x2 + 1), range(y1, y2 + 1), z1 - 1]
    else:
        onebelow = space[range(x1, x2 + 1), range(y1, y2 + 1), range(z1 - 1, z2 - 1 + 1)]
    maxsupport = onebelow.max()
    minsupport = np.where(onebelow < 0, 10000, onebelow).min()
    if minsupport == maxsupport:
        if maxsupport in droppable:
            droppable.remove(maxsupport)
    support[idx] = set(o for o in onebelow if o >= 0)

print(len(droppable))

p1blocks = copy.deepcopy(blocks)
total = 0
for p1idx in range(len(p1blocks)):
    dropping = set([p1idx])
    odropping = set([])
    while odropping != dropping:
        odropping = copy.copy(dropping)
        for idx in range(len(p1blocks)):
            if idx in dropping:
                continue
            if support[idx] <= dropping:
                dropping.add(idx)
    total += len(dropping) - 1
print(total)
