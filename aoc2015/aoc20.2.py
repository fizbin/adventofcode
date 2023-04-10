import numpy as np
import sys

goal = int(sys.argv[1]) if len(sys.argv) > 1 else 29000000

boxnum = 1_000_000
bigarray = np.zeros(shape=(boxnum,))

for j in range(1, boxnum):
    bigarray[np.arange(0, min(boxnum, 50*j + 1), j)] += j

bigarray[0] = 0
print(np.flatnonzero(bigarray*11 >= goal).min())
