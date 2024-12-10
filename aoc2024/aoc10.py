from aoc_util import get_data_lines
import numpy as np
from scipy.ndimage import convolve

npgrid = np.genfromtxt(get_data_lines(10), dtype=int, delimiter=1)


def find_reach(spot):
    cgrid = np.zeros_like(npgrid)
    cgrid[spot] = 1
    for tgt in range(1, 10):
        gridnb = convolve(
            cgrid, np.array([[0, 1, 0], [1, 0, 1], [0, 1, 0]]), mode="constant", cval=0
        )
        cgrid = (gridnb != 0) * (npgrid == tgt).astype(int)

    return (cgrid != 0).sum()


total = 0
for x in range(npgrid.shape[0]):
    for y in range(npgrid.shape[1]):
        if npgrid[x, y] == 0:
            total += find_reach((x, y))

print("Part 1:", total)

cgrid = np.zeros_like(npgrid) + 1
cgrid = cgrid * (npgrid == 9).astype(int)
for tgt in range(8, -1, -1):
    gridnb = convolve(cgrid, np.array([[0, 1, 0], [1, 0, 1], [0, 1, 0]]), mode="constant", cval=0)
    cgrid = gridnb * (npgrid == tgt).astype(int)

print("Part 2:", cgrid.sum())
