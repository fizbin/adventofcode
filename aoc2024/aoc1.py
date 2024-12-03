from aoc_util import get_data_lines
import numpy as np

nprec = np.genfromtxt(get_data_lines(1), dtype=int)
a = nprec.transpose()
b, c = tuple(a)
b.sort()
c.sort()
print("Part 1:", abs(b - c).sum())
bp = b.reshape(b.size, 1)
print("Part 2:", ((bp == c.reshape(1, b.size)) * bp).sum())
