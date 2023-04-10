import numpy as np
import sys
import re

with open('aoc16.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

initial = [int(x) for x in re.findall(r'\d', data[0])]
work = np.array(initial)
permutation = np.fromfunction(
    lambda x, y: ((0+1j)**((y+1)//(x+1))).imag,
    shape=(len(initial), len(initial))).astype(int)

for _ in range(100):
    work = abs((permutation * work).sum(axis=1)) % 10

print(work[0:8])
