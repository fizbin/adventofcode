import re
import sys
import numpy as np

with open("aoc18.in.txt" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

lifegrid = np.zeros((102, 102), dtype=int)
lifegrid[1:101, 1:101] = np.array([[1 if c == '#' else 0 for c in ln]
                                   for ln in data])

for _ in range(100):
    nextgen = np.zeros((100, 100), dtype=int)
    for xadj in (-1, 0, 1):
        for yadj in (-1, 0, 1):
            mult = 10 if (xadj == 0 and yadj == 0) else 1
            nextgen += lifegrid[1+xadj:101+xadj, 1+yadj:101+yadj]*mult
    nextgen = ((nextgen == 3) | (nextgen == 12) | (nextgen == 13)).astype(int)
    lifegrid[1:101, 1:101] = nextgen

print(lifegrid.sum())
