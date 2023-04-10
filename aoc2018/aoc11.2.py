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
    rack[0, :] = 0
    rack[:, 0] = 0
    tots = np.zeros(shape=rack.shape + (301,), dtype=int)
    tots[:, :, 1] = rack
    for sz in range(2, 301):
        tots[:, :, sz] = tots[:, :, sz-1]
        for s in range(sz):
            tots[:, :, sz] += np.roll(rack, shift=(-sz+1, -s), axis=(0, 1))
            tots[:, :, sz] += np.roll(rack, shift=(-s, -sz+1), axis=(0, 1))
        tots[:, :, sz] -= np.roll(rack, shift=(-sz+1, -sz+1), axis=(0, 1))
        for s in range(sz):
            tots[-s, :, sz] = 0
            tots[:, -s, sz] = 0
        tots[0, :, sz] = 0
        tots[:, 0, sz] = 0

    ind = np.unravel_index(np.argmax(tots, axis=None), tots.shape)
    print(tots[ind])
    return ind

print(compute(1718))
