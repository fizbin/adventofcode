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

attained_per = [
    {np.array((pos[..., c], vel[..., c])).tostring(): 0}
    for c in (0, 1, 2)]
stepnum = 0
try:
    while True:
        stepnum += 1
        acc = np.sum(np.sign(pos - pos[acc_getter]), axis=1)
        vel += acc
        pos += vel
        for c in (0, 1, 2):
            attained = attained_per[c]
            state = np.array((pos[..., c], vel[..., c])).tostring()
            if state in attained:
                print("Repeat on %d after step %d (%d)" % (
                    c, stepnum, attained[state]))
                attained.clear()
            attained[state] = stepnum
except:
    print("Broke at %d" % stepnum)
    raise

#energy = (np.sum(abs(pos), axis=1) * np.sum(abs(vel), axis=1)).sum(axis=0)
#print(energy)
