#!/usr/bin/python

import aoc_util
import numpy as np
import collections
from scipy.ndimage import convolve

data = aoc_util.get_data(21)
grid = aoc_util.chargrid(data)

chargrid = np.array(grid, dtype=np.str_)
mask = np.where(chargrid != "#", 1, 0).astype(bool)
elfpos = np.where(chargrid == "S", 1, 0).astype(bool)
convkernel = np.array([[0, 1, 0], [1, 0, 1], [0, 1, 0]], dtype=int)

for step in range(64):
    convolve(elfpos, convkernel, mode="constant", cval=0, output=elfpos)
    elfpos[...] *= mask

print(elfpos.sum())

for step in range(1000):
    convolve(elfpos, convkernel, mode="constant", cval=0, output=elfpos)
    elfpos[...] *= mask

elfgridA = elfpos.copy()
convolve(elfpos, convkernel, mode="constant", cval=0, output=elfpos)
elfpos[...] *= mask
elfgridB = elfpos.copy()

elfA_on_odd = set()
set_grid_spots = set()
dims = chargrid.shape
elfpos = dict([(0j, np.where(chargrid == "S", 1, 0).astype(bool))])
convtmp = np.zeros_like(elfpos[0j])
def calc_size(step):
    retval = sum(v.sum() for (idx,v) in elfpos.items() if idx not in set_grid_spots)
    if step % 2:
        retval += len(elfA_on_odd)*elfgridA.sum() + (len(set_grid_spots) - len(elfA_on_odd))*elfgridB.sum()
    else:
        retval += len(elfA_on_odd)*elfgridB.sum() + (len(set_grid_spots) - len(elfA_on_odd))*elfgridA.sum()
    return retval
for step in range(501): # 26501365):
    # if step % 100 == 0:
    #     print("DBG", step, len(elfpos), len(set_grid_spots), len([x for x in elfpos if x in set_grid_spots]))
    if step in (1, 6, 10, 50, 100, 500, 1000, 5000):
        print(f"after {step}, have {calc_size(step)} (from {len(elfpos)})")
        print(f"DBG:", set_grid_spots)
        print(f"DBG:", elfA_on_odd, elfgridA.sum(), elfgridB.sum())
        sumline = dict((gidx, grid.sum()) for (gidx, grid) in elfpos.items())
        for x in set_grid_spots:
            if (x in elfA_on_odd) ^ (step % 2 == 0):
                sumline[x] = elfgridA.sum()
            else:
                sumline[x] = elfgridB.sum()
        print("DBG:", sumline)
        for xpart in range(-10,10):
            for ypart in range(-10,10):
                if sumline.get(xpart + ypart*1j, 0) != 0:
                    print("DBG:", (xpart,ypart), sumline.get(xpart + ypart*1j))
        print("DBG:", sum(v for v in sumline.values()))
    nelfpos = collections.defaultdict(lambda: np.zeros_like(convtmp))
    nset_spots = set()
    n_gridA_on_odd = set()
    for grididx, elfgrid in elfpos.items():
        if grididx in set_grid_spots:
            if (
                (grididx + 1) not in set_grid_spots
                or (grididx + 1) not in set_grid_spots
                or (grididx + 1) not in set_grid_spots
                or (grididx + 1) not in set_grid_spots
            ):
                if (grididx in elfA_on_odd) == (0 == step % 2):
                    nelfpos[grididx][...] = elfgridA
                else:
                    nelfpos[grididx][...] = elfgridB
        else:
            nelfgrid = nelfpos[grididx]
            convolve(elfgrid, convkernel, mode="constant", cval=0, output=convtmp)
            convtmp[...] *= mask
            nelfgrid[...] += convtmp
            if (nelfgrid == elfgridA).all():
                nset_spots.add(grididx)
                if step % 2 == 0:
                    n_gridA_on_odd.add(grididx)

        if elfgrid[0, :].any() and ((grididx - 1) not in set_grid_spots):
            nelfpos[grididx - 1][-1, :] += elfgrid[0, :]
        if elfgrid[-1, :].any() and ((grididx + 1) not in set_grid_spots):
            nelfpos[grididx + 1][0, :] += elfgrid[-1, :]
        if elfgrid[:, 0].any() and ((grididx - 1j) not in set_grid_spots):
            nelfpos[grididx - 1j][:, -1] += elfgrid[:, 0]
        if elfgrid[:, -1].any() and ((grididx + 1j) not in set_grid_spots):
            nelfpos[grididx + 1j][:, 0] += elfgrid[:, -1]
    set_grid_spots.update(nset_spots)
    elfA_on_odd.update(n_gridA_on_odd)
    elfpos = nelfpos

print(calc_size(100 )) # 26501365))
