from aoc_util import numbers, get_data_lines
import numpy as np


def neigh3d(spot):
    for dx in (-1, 1):
        yield (spot[0] + dx, spot[1], spot[2])
    for dy in (-1, 1):
        yield (spot[0], spot[1] + dy, spot[2])
    for dz in (-1, 1):
        yield (spot[0], spot[1], spot[2] + dz)


data = get_data_lines(18)
spots = [tuple(f + 2 for f in numbers(line)) for line in data]
spotss = set(spots)

tot = 0
for spot in spots:
    for ne in neigh3d(spot):
        if ne not in spotss:
            tot += 1
print(tot)

# part 2

thingy = np.ones((25, 25, 25), dtype=int)
for spot in spots:
    thingy[spot] = 999999

thingy[0, :, :] = 0

thingy2 = np.ones_like(thingy) * 999999
while (thingy != thingy2).any():
    thingy2[:, :] = thingy
    rot = np.roll(thingy2, 1, 0)
    thingy = np.where((rot < thingy) & (thingy < 999999), rot, thingy)
    rot = np.roll(thingy2, -1, 0)
    thingy = np.where((rot < thingy) & (thingy < 999999), rot, thingy)
    rot = np.roll(thingy2, 1, 1)
    thingy = np.where((rot < thingy) & (thingy < 999999), rot, thingy)
    rot = np.roll(thingy2, -1, 1)
    thingy = np.where((rot < thingy) & (thingy < 999999), rot, thingy)
    rot = np.roll(thingy2, 1, 2)
    thingy = np.where((rot < thingy) & (thingy < 999999), rot, thingy)
    rot = np.roll(thingy2, -1, 2)
    thingy = np.where((rot < thingy) & (thingy < 999999), rot, thingy)

tot = 0
for spot in spots:
    for ne in neigh3d(spot):
        if thingy[ne] == 0:
            tot += 1
print(tot)
