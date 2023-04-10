import numpy as np

def compute(serial):
    def rackfunc(x, y):
        rackid = x + 10
        power_level = rackid * y
        power_level += serial
        power_level *= rackid
        power_level %= 1000
        power_level = power_level // 100
        power_level -= 5
        return power_level

    rack = np.fromfunction(rackfunc, shape=(301, 301), dtype=int)
    tots = np.zeros(shape=rack.shape, dtype=int)
    for x in [0, 1, 2]:
        for y in [0, 1, 2]:
            tots += np.roll(rack, shift=(-x, -y), axis=(0, 1))
    tots[:, -1] = 0
    tots[:, -2] = 0
    tots[-1, :] = 0
    tots[-2, :] = 0
    ind = np.unravel_index(np.argmax(tots, axis=None), tots.shape)
    print(rack[ind[0]-1:ind[0]+4,ind[1]-1:ind[1]+4])
    return ind

