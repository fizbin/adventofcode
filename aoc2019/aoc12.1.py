import numpy as np
import re
import sys

with open('aoc12.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

arrs = [[int(xstr) for xstr in re.findall(r'-?\d+', line)]
        for line in data]
pos = np.reshape(arrs, (-1, 3))
vel = np.zeros(pos.shape, dtype=int)
acc_getter = np.array([list(range(vel.shape[0]))]*vel.shape[0]).transpose()
print(acc_getter)

for stepnum in range(1000):
    acc = np.sum(np.sign(pos - pos[acc_getter]), axis=1)
    vel += acc
    pos += vel

energy = (np.sum(abs(pos), axis=1) * np.sum(abs(vel), axis=1)).sum(axis=0)
print(energy)
