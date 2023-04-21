import sys
import numpy as np
from scipy.ndimage import convolve


infile = "aoc18.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = infilep.read().strip()

narr = np.zeros((40, len(data)), dtype=int)
for idx, char in enumerate(data):
    narr[0, idx] = 1 if char == "^" else 0

kernel = np.array([[0, 0, 0], [0, 0, 0], [1, 0, 1]], dtype=int)
for _ in range(39):
    tmp = convolve(narr, kernel, mode="constant", cval=0)
    narr[...] = np.where((narr == 1) | (tmp == 1), 1, 0)
print((1 - narr).sum())

p2arr = narr[0:2, :]
ans = 0
for _ in range(400000):
    ans += (1 - p2arr[0, :]).sum()
    tmp = convolve(p2arr, kernel, mode="constant", cval=0)
    p2arr[0, :] = (tmp[1, :] == 1).astype(int)

print(ans)
