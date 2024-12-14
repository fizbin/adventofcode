from aoc_util import get_data_lines
import numpy as np
import scipy.ndimage
import itertools


# convenience to change defaults
def convolve(input, weights, output=None, mode="constant", cval=0, origin=0):
    return scipy.ndimage.convolve(
        input, weights, output=output, mode=mode, cval=cval, origin=origin
    )


def neighbors(shape, loc):
    retval = []
    if loc[0] > 0:
        retval.append((loc[0] - 1, loc[1]))
    if loc[1] > 0:
        retval.append((loc[0], loc[1] - 1))
    if loc[0] < shape[0] - 1:
        retval.append((loc[0] + 1, loc[1]))
    if loc[1] < shape[1] - 1:
        retval.append((loc[0], loc[1] + 1))
    return retval


npgrid = np.genfromtxt(get_data_lines(12), dtype=str, delimiter=1)

regions = np.fromiter(
    itertools.count(1), count=npgrid.shape[0] * npgrid.shape[1], dtype=int
).reshape(npgrid.shape)
done = False
while not done:
    done = True
    for x in range(npgrid.shape[0] - 1, -1, -1):
        for y in range(npgrid.shape[1] - 1, -1, -1):
            spot = (x, y)
            for nbr in neighbors(npgrid.shape, spot):
                if regions[nbr] != regions[spot] and npgrid[nbr] == npgrid[spot]:
                    nval = max(regions[nbr], regions[spot])
                    done = False
                    regions[nbr] = nval
                    regions[spot] = nval

total = 0
for rgnum in set(list(regions.flatten())):
    mygrid = (regions == rgnum).astype(int)
    fencesz = 0
    area = mygrid.sum()
    fence = convolve(mygrid, np.array([[0, -1, 0], [0, 1, 0], [0, 0, 0]]))
    fencesz += (fence == 1).astype(int).sum()
    fence = convolve(mygrid, np.array([[0, 0, 0], [-1, 1, 0], [0, 0, 0]]))
    fencesz += (fence == 1).astype(int).sum()
    fence = convolve(mygrid, np.array([[0, 0, 0], [0, 1, -1], [0, 0, 0]]))
    fencesz += (fence == 1).astype(int).sum()
    fence = convolve(mygrid, np.array([[0, 0, 0], [0, 1, 0], [0, -1, 0]]))
    fencesz += (fence == 1).astype(int).sum()
    total += area * fencesz

print("Part 1:", total)

total = 0
trgnum = regions[0, 0]
for rgnum in sorted(set(list(regions.flatten()))):
    mygrid = (regions == rgnum).astype(int)
    area = mygrid.sum()
    edge_detect = convolve(1 - mygrid, np.array([[0, 1, 0], [2, 0, 4], [0, 8, 0]]), cval=1) * mygrid
    edge_detect2 = convolve(edge_detect, np.array([[0, 16, 0], [256, 1, 0], [0, 0, 0]])) * mygrid
    nedges = 0
    nedges += ((edge_detect2 & (1 | 256)) == 1).astype(int).sum()
    nedges += ((edge_detect2 & (8 | (8 * 256))) == 8).astype(int).sum()
    nedges += ((edge_detect2 & (2 | (2 * 16))) == 2).astype(int).sum()
    nedges += ((edge_detect2 & (4 | (4 * 16))) == 4).astype(int).sum()
    total += area * nedges

print("Part 2:", total)
