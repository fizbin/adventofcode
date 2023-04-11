#! python

import sys
import math
import numpy as np

my_input = 312051
if len(sys.argv) > 1:
    my_input = int(sys.argv[1])

size = int(math.sqrt(my_input)) + 21
size = size + (size % 2) + 1

square = np.zeros(shape=(size, size), dtype=int)

loc = (size // 2, size // 2)
direction = (1, 0)
square[loc] = 1

step_size = 1
val = 1
while not (square == my_input).any():
    for _ in range(step_size):
        loc = (loc[0] + direction[0], loc[1] + direction[1])
        val += 1
        square[loc] = val
    direction = (-direction[1], direction[0])
    for _ in range(step_size):
        loc = (loc[0] + direction[0], loc[1] + direction[1])
        val += 1
        square[loc] = val
    direction = (-direction[1], direction[0])
    step_size += 1

spot = np.argwhere(square == my_input)[0]
print(abs(spot[0] - size // 2) + abs(spot[1] - size // 2))
square = np.zeros_like(square)

loc = (size // 2, size // 2)
direction = (1, 0)
square[loc] = 1
step_size = 1
while not (square > my_input).any():
    for _ in range(step_size):
        loc = (loc[0] + direction[0], loc[1] + direction[1])
        square[loc] = square[loc[0] - 1 : loc[0] + 2, loc[1] - 1 : loc[1] + 2].sum()
    direction = (-direction[1], direction[0])
    for _ in range(step_size):
        loc = (loc[0] + direction[0], loc[1] + direction[1])
        square[loc] = square[loc[0] - 1 : loc[0] + 2, loc[1] - 1 : loc[1] + 2].sum()
    direction = (-direction[1], direction[0])
    step_size += 1
big = square.max()
print(np.where(square > my_input, square, big).min())
