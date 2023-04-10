import numpy as np

coordl = np.loadtxt('ac6.in.txt', delimiter=', ', dtype=int)
coordlt = coordl.transpose()


def distfunc(s, x, y):
    return abs(-500 + x - coordlt[0][s]) + abs(-500 + y - coordlt[1][s])


dists = np.fromfunction(distfunc, ((coordl.shape)[0], 2000, 2000), dtype=int)

print((dists.sum(axis=0) < 10000).astype(int).sum())
