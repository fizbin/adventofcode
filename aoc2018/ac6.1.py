import numpy as np

coordl = np.loadtxt('ac6.in.txt', delimiter=', ', dtype=int)
coordlt = coordl.transpose()


def distfunc(s, x, y):
    return abs(-500 + x - coordlt[0][s]) + abs(-500 + y - coordlt[1][s])


dists = np.fromfunction(distfunc, ((coordl.shape)[0], 2000, 2000), dtype=int)

mins = dists.min(axis=0)

eqmins = (dists == mins)

owned = (eqmins.astype(int).sum(axis=0) == 1) & eqmins

edge_lords = (owned[:, 0, :].any(axis=1) |
              owned[:, -1, :].any(axis=1) |
              owned[:, :, -1].any(axis=1) |
              owned[:, :, 0].any(axis=1)).reshape(((coordl.shape)[0], 1, 1))

print((owned & ~edge_lords).astype(int).sum(axis=(1, 2)).max())
