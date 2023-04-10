from aoc_util import chargrid, get_data
import numpy as np
from scipy.ndimage import convolve

kernel_allnbs = np.array([[1, 1, 1], [1, 0, 1], [1, 1, 1]], dtype=int)
kernel_orthonbs = np.array([[0, 1, 0], [1, 0, 1], [0, 1, 0]], dtype=int)
kernel_snbs = np.zeros_like(kernel_allnbs)
kernel_snbs[-1, :] = 1
kernel_nnbs = np.zeros_like(kernel_allnbs)
kernel_nnbs[0, :] = 1
kernel_enbs = np.zeros_like(kernel_allnbs)
kernel_enbs[:, -1] = 1
kernel_wnbs = np.zeros_like(kernel_allnbs)
kernel_wnbs[:, 0] = 1

bcast_n = (kernel_snbs, np.array([[0, 2, 0], [0, 1, 0], [0, 0, 0]], dtype=int))
bcast_s = (kernel_nnbs, np.array([[0, 0, 0], [0, 1, 0], [0, 2, 0]], dtype=int))
bcast_e = (kernel_wnbs, np.array([[0, 0, 0], [0, 1, 3], [0, 0, 0]], dtype=int))
bcast_w = (kernel_enbs, np.array([[0, 0, 0], [3, 1, 0], [0, 0, 0]], dtype=int))


def elfstep(elves, tick):
    if elves[0, :].any() or elves[:, 0].any() or elves[-1, :].any() or elves[:, -1].any():
        elves = np.pad(elves, 1, mode="constant")
    mobile_elves = (elves * convolve(elves, kernel_allnbs, mode="constant", cval=0) > 0).astype(int)
    if mobile_elves.any():
        dirtrials = [bcast_n, bcast_s, bcast_w, bcast_e, bcast_n, bcast_s, bcast_w][
            tick % 4 : 4 + (tick % 4)
        ]
        proposed = np.zeros_like(elves)
        for (blockers_kernel, bcast_kernel) in dirtrials:
            proposers = mobile_elves * (
                1
                - convolve(elves, blockers_kernel, mode="constant", cval=0).astype(bool).astype(int)
            )
            proposed += convolve(proposers, bcast_kernel, mode="constant", cval=0).astype(int)
            mobile_elves -= proposers

        # n-s proposal collisions
        col4 = (proposed == 4).nonzero()
        proposed[col4[0] - 1, col4[1]] = 0
        proposed[col4[0], col4[1]] = 0
        proposed[col4[0] + 1, col4[1]] = 0

        # e-w proposal collisions
        col6 = (proposed == 6).nonzero()
        proposed[col6[0], col6[1] - 1] = 0
        proposed[col6[0], col6[1]] = 0
        proposed[col6[0], col6[1] + 1] = 0

        elves ^= (proposed > 0).astype(int)
        # print("dbg:", elves.sum())
        return elves
    return None


data = chargrid(get_data(23))

elves = (np.array(data, dtype=np.str_) == "#").astype(int)

for tick in range(10):
    elves = elfstep(elves, tick)

elfspots = elves.nonzero()
minx = int(min(elfspots[0]))
maxx = int(max(elfspots[0]))
miny = int(min(elfspots[1]))
maxy = int(max(elfspots[1]))

# print((maxx, maxy, minx, miny, elves.max(), elves.min(), elves.sum()))
# print(elfspots)
print((maxx - minx + 1) * (maxy - miny + 1) - elves.sum())

elves = (np.array(data, dtype=np.str_) == "#").astype(int)
tick = 1
while True:
    elves = elfstep(elves, tick - 1)
    if elves is None:
        print(tick)
        break
    tick += 1
