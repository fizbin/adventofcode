import sys
import numpy as np

with open('aoc24.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

a = [list(x.replace('\n', '')) for x in data]
b = [[ch == '#' for ch in row] for row in a]
c = np.array(b, dtype=int)
print(c)

planet = np.zeros((405, 5, 5), dtype=int)
planet[202] = c

for generation in range(200):
    tplanet = planet.copy()
    for layer in range(1, 404):
        c = planet[layer].copy()
        clft = np.roll(c, 1, axis=0)
        clft[0] = planet[layer-1, 1, 2]
        clft[3, 2] = sum(planet[layer+1, -1, ...])

        crgt = np.roll(c, -1, axis=0)
        crgt[-1] = planet[layer-1, 3, 2]
        crgt[1, 2] = sum(planet[layer+1, 0, ...])

        cup = np.roll(c, 1, axis=1)
        cup[..., 0] = planet[layer-1, 2, 1]
        cup[2, 3] = sum(planet[layer+1, ..., -1])

        cdn = np.roll(c, -1, axis=1)
        cdn[..., -1] = planet[layer-1, 2, 3]
        cdn[2, 1] = sum(planet[layer+1, ..., 0])

        nbd = clft + cup + cdn + crgt
        c = (c * ((nbd == 1).astype(int)) +
             (1-c) * ((nbd > 0).astype(int)) * ((nbd < 3).astype(int)))
        c[2, 2] = 0
        tplanet[layer] = c

    planet = tplanet
    print(generation)

print(np.sum(planet))
