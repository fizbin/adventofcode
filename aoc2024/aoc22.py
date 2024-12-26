import numpy as np
from aoc_util import get_data_lines

data = np.genfromtxt(get_data_lines(22), dtype=int)


def advance(n):
    n = n ^ (n * 64)
    n %= 16777216
    n = n ^ (n // 32)
    n %= 16777216
    n = n ^ (n * 2048)
    n %= 16777216
    return n


working = np.copy(data)
grid = np.zeros(shape=(working.size, 2001, 5), dtype=int)
grid[:, 0, 0] = working[:] % 10
for col in range(1, 2001):
    working = advance(working)
    grid[:, col, 0] = working[:] % 10
print(working.sum())

grid[:, :, 1] = grid[:, :, 0] - np.roll(grid[:, :, 0], shift=1, axis=1)
grid[:, :, 2] = np.roll(grid[:, :, 1], shift=1, axis=1)
grid[:, :, 3] = np.roll(grid[:, :, 2], shift=1, axis=1)
grid[:, :, 4] = np.roll(grid[:, :, 3], shift=1, axis=1)

grand_max = 0
max_bananas: dict[tuple[int, ...], int] = {}
for buyer in range(data.size):
    my_bananas = {}
    for start in reversed(list(range(4, 2001))):
        my_bananas[tuple(grid[buyer, start, 1:])] = grid[buyer, start, 0]
    for k, v in my_bananas.items():
        max_bananas[k] = max_bananas.get(k, 0) + v
        grand_max = max(grand_max, max_bananas[k])

print(grand_max)
