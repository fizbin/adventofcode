import sys
import numpy as np

pts = np.loadtxt('aoc25.in.txt' if len(sys.argv) < 2 else sys.argv[1],
                 dtype=int, delimiter=',')

pts_d = np.fromfunction(lambda x, y: abs(pts[x] - pts[y]).sum(axis=2),
                        dtype=int, shape=(pts.shape[0], pts.shape[0]))

pts_adj = (pts_d <= 3).astype(int)

while True:
    pts_adj2 = (pts_adj @ pts_adj).astype(bool).astype(int)
    if (pts_adj2 == pts_adj).all():
        break
    pts_adj = pts_adj2

print(len(set(tuple(x) for x in pts_adj)))
