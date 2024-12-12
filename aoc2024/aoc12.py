from aoc_util import get_data_lines
import numpy as np
import scipy.ndimage


# convenience to change defaults
def convolve(input, weights, output=None, mode="constant", cval=0, origin=0):
    return scipy.ndimage.convolve(input, weights, output=output, mode=mode, cval=cval, origin=0)


npgrid = np.genfromtxt(get_data_lines(12), dtype=str, delimiter=1)

asnumgrid = np.array(list(range(npgrid.shape[0] * npgrid.shape[1]))).reshape(npgrid.shape) + 1

asnumgrid = np.zeros(shape=npgrid.shape, dtype=int)
ltr = npgrid[0][0]
n = 1
for x in range(npgrid.shape[0]):
    n = 1 + x * npgrid.shape[1]
    for y in range(npgrid.shape[1]):
        if ltr != npgrid[x][y]:
            n += 1
            ltr = npgrid[x][y]
        if x > 0 and npgrid[x - 1][y] == npgrid[x][y] and asnumgrid[x - 1][y] != n:
            val1 = asnumgrid[x - 1][y]
            val2 = n
            asnumgrid = np.where(asnumgrid == val1, val2, asnumgrid)
        asnumgrid[x][y] = n

total = 0
for rgnum in set(list(asnumgrid.flatten())):
    mygrid = (asnumgrid == rgnum).astype(int)
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

print(total)


def count_regions(mygrid):
    outgrid = np.zeros_like(mygrid, dtype=int)
    for x in range(mygrid.shape[0]):
        n = 1 + x * mygrid.shape[1]
        for y in range(mygrid.shape[1]):
            if not mygrid[x][y]:
                n += 1
                continue
            if x > 0 and mygrid[x - 1][y] and outgrid[x - 1][y] != n:
                val1 = outgrid[x - 1][y]
                val2 = n
                outgrid = np.where(outgrid == val1, val2, outgrid)
            outgrid[x][y] = n
    return len(set(x for x in outgrid.flatten() if x != 0))


total = 0
trgnum = asnumgrid[0, 0]
for rgnum in sorted(set(list(asnumgrid.flatten()))):
    mygrid = (asnumgrid == rgnum).astype(int)
    area = mygrid.sum()
    edge_detect = convolve(1 - mygrid, np.array([[0, 1, 0], [2, 0, 4], [0, 8, 0]]), cval=1) * mygrid
    edge_detect2 = convolve(edge_detect, np.array([[0, 16, 0], [256, 1, 0], [0, 0, 0]])) * mygrid
    nedges = 0
    nedges += ((edge_detect2 & (1 | 256)) == 1).astype(int).sum()
    nedges += ((edge_detect2 & (8 | (8 * 256))) == 8).astype(int).sum()
    nedges += ((edge_detect2 & (2 | (2 * 16))) == 2).astype(int).sum()
    nedges += ((edge_detect2 & (4 | (4 * 16))) == 4).astype(int).sum()
    total += area * nedges

print(total)
