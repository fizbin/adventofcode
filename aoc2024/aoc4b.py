from aoc_util import get_data_lines
import numpy as np
from scipy.ndimage import convolve

a = np.genfromtxt(get_data_lines(4), dtype=str, delimiter=1)
kernel_horiz = np.array([[0,0,0,0,0], [0,0,0,0,0], [0,1,10,100,1000], [0,0,0,0,0], [0,0,0,0,0]], dtype=int)
kernel_vert = np.transpose(kernel_horiz)
kernel_diag1 = np.array([[0,0,0,0,0], [0,1,0,0,0], [0,0,10,0,0], [0,0,0,100,0], [0,0,0,0,1000]], dtype=int)
kernel_diag2 = kernel_diag1[::-1]
anum = (a == 'X') + 2*(a == 'M') + 3*(a=='A') + 4*(a=='S')
total = 0
for kernel in (kernel_horiz, kernel_vert, kernel_diag1, kernel_diag2):
    c = convolve(anum, kernel, mode='constant', cval=0)
    total += (c == 1234).sum()
    total += (c == 4321).sum()
print(f"Part 1: {total}")

anum2 = 2*(a == 'M') + 1*(a=='A') + 3*(a=='S')
c = convolve(anum2, np.array([[10,0,100],[0,1,0],[100,0,10]]), mode='constant',cval=0)
print(f"Part 2: {(c == 551).sum()}")