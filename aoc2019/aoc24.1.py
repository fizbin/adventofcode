import sys
import re
import numpy as np


def shift1(arr, num, fill_value=np.nan):
    arr = np.roll(arr,num)
    if num < 0:
        arr[num:] = fill_value
    elif num > 0:
        arr[:num] = fill_value
    return arr


with open('aoc24.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

a = [list(x.replace('\n','')) for x in data]
b = [[ch == '#' for ch in row] for row in a]
c = np.array(b, dtype=int)
print(c)


bdarray = np.reshape(list(2**n for n in range(25)), (5,5))

def getsig(arr):
    return np.sum(bdarray*arr)

sigs = set()
while True:
    sig = getsig(c)
    if sig in sigs:
        print(sig)
        break
    sigs.add(sig)
    clft = np.roll(c, 1, axis=0)
    clft[0] = 0
    crgt = np.roll(c, -1, axis=0)
    crgt[-1] = 0
    cup = np.roll(c, 1, axis=1)
    cup[..., 0] = 0
    cdn = np.roll(c, -1, axis=1)
    cdn[..., -1] = 0
    nbd = clft + cup + cdn + crgt
    c = c * ((nbd == 1).astype(int)) + (1-c) * ((nbd > 0).astype(int)) * ((nbd < 3).astype(int))

