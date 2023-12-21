#!/usr/bin/python

import aoc_util
from functools import cache
from aoc_util import numpy_shift
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

mydims = mask.shape

@cache
def find_elf_dist(vonspot):
    elfsteps = np.zeros_like(mask.astype(int)) + 10000
    psum = -10
    elfsteps[vonspot] = 0
    while psum != elfsteps.sum():
        psum = elfsteps.sum()
        elfstepsP1 = elfsteps + 1
        elfsteps = np.array([numpy_shift(elfstepsP1, 1, 0, 10000),
                            numpy_shift(elfstepsP1, -1, 0, 10000),
                            numpy_shift(elfstepsP1, -1, 1, 10000),
                            numpy_shift(elfstepsP1, 1, 1, 10000),
                            elfsteps
                            ]).min(axis=0)
        elfsteps = np.where(mask, elfsteps, 10000)
    return elfsteps

manual_elf_coverage_cache = {}
def elf_coverage(vonspot, remaining_steps):
    if remaining_steps < 0:
        return 0
    if remaining_steps >= 500:
        return elf_coverage(vonspot, 490 - (remaining_steps % 2))
    if (vonspot, remaining_steps) in manual_elf_coverage_cache:
        return manual_elf_coverage_cache[vonspot, remaining_steps]
    
    retval = 0

    elfdist = find_elf_dist(vonspot)
    retval = ((elfdist <= remaining_steps) * (elfdist % 2 == remaining_steps % 2)).sum()
    if remaining_steps < 500:
        manual_elf_coverage_cache[vonspot, remaining_steps] = retval
    return retval

s_spot = tuple(np.transpose(np.nonzero(chargrid == 'S'))[0])

fed = find_elf_dist(s_spot)
print(fed, np.where(fed < 10000, fed, 0).max(), fed[0,:].max(), fed[:,0].max(), fed[-1,:].max(), fed[:,-1].max())
print(26501365 // mydims[0])

target_steps = 26501365
elf_dist_0 = find_elf_dist(s_spot)
retval = 0
for grid_x_disp in range(target_steps // mydims[0] - 3, target_steps // mydims[0] + 3):
    if grid_x_disp < 0:
        dist_to_mid_yd_0 = elf_dist_0[0].min() + 1 + (-1 - grid_x_disp) * mydims[1]
        dist_to_br_yd_n1 = dist_to_mid_yd_0 + (mydims[0] + 1) // 2
        dist_to_bl_yd_1 = dist_to_br_yd_n1
        min_yd = (target_steps - dist_to_br_yd_n1) // mydims[0] - 3
        max_yd = abs(min_yd)
        for grid_y_disp in range(min_yd, max_yd + 1):
            if grid_y_disp < 0:
                # from bl corner
                dist_to_br = dist_to_br_yd_n1 + (-1 - grid_y_disp) * mydims[1]
                retval += elf_coverage((mydims[0]-1,mydims[1]-1), target_steps - dist_to_br)
            elif grid_y_disp > 0:
                dist_to_bl = dist_to_bl_yd_1 + (grid_y_disp - 1) * mydims[1]
                retval += elf_coverage((mydims[0]-1,0), target_steps - dist_to_bl)
            else:  # 0
                retval += elf_coverage((mydims[0]-1, s_spot[1]), target_steps - dist_to_mid_yd_0)
    elif grid_x_disp > 0:
        dist_to_mid_yd_0 = elf_dist_0[-1].min() + 1 + (grid_x_disp - 1) * mydims[1]
        dist_to_tr_yd_n1 = dist_to_mid_yd_0 + (mydims[0] + 1) // 2
        dist_to_tl_yd_1 = dist_to_tr_yd_n1
        min_yd = (target_steps - dist_to_tr_yd_n1) // mydims[0] - 3
        max_yd = abs(min_yd)
        for grid_y_disp in range(min_yd, max_yd + 1):
            if grid_y_disp < 0:
                # from bl corner
                dist_to_tr = dist_to_tr_yd_n1 + (-1 - grid_y_disp) * mydims[1]
                retval += elf_coverage((mydims[0]-1,mydims[1]-1), target_steps - dist_to_tr)
            elif grid_y_disp > 0:
                dist_to_tl = dist_to_tl_yd_1 + (grid_y_disp - 1) * mydims[1]
                retval += elf_coverage((mydims[0]-1,0), target_steps - dist_to_tl)
            else:  # 0
                retval += elf_coverage((mydims[0]-1, s_spot[1]), target_steps - dist_to_mid_yd_0)
    else: # grid_x_0
        dist_to_mr_yd_n1 = elf_dist_0[:,0].min() + 1
        dist_to_ml_yd_1 = elf_dist_0[:,-1].min() + 1
        for grid_y_disp in range(target_steps // mydims[0] - 3, target_steps // mydims[0] + 3):
            if grid_y_disp < 0:
                dist_to_mr = dist_to_mr_yd_n1 + (-1 - grid_y_disp) * mydims[1]
                retval += elf_coverage((s_spot[0],mydims[1]-1), target_steps - dist_to_mr)
            elif grid_y_disp > 0:
                dist_to_ml = dist_to_ml_yd_1 + (grid_y_disp - 1) * mydims[1]
                retval += elf_coverage((s_spot[0], 0), target_steps - dist_to_ml)
            else:
                retval += elf_coverage(s_spot, target_steps)

print(retval)
