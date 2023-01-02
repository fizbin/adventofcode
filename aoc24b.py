from aoc_util import *
import numpy as np
from scipy.ndimage import convolve

data = chargrid(get_data(24))

pitch = np.array(data, dtype=np.str_)[1:-1, 1:-1]
s_blizzards = (pitch != "v").astype(int)
n_blizzards = (pitch != "^").astype(int)
e_blizzards = (pitch != ">").astype(int)
w_blizzards = (pitch != "<").astype(int)
kernel = np.array([[0, 1, 0], [1, 1, 1], [0, 1, 0]], dtype=int)


def find_exit(initial, goal, initial_time: int):
    working = np.zeros(pitch.shape, dtype=int)
    t = initial_time + 1
    while True:
        working = convolve(working, kernel, mode="constant", cval=0).astype(bool).astype(int)
        working[initial] = 1
        working *= np.roll(s_blizzards, t, axis=0)
        working *= np.roll(n_blizzards, -t, axis=0)
        working *= np.roll(e_blizzards, t, axis=1)
        working *= np.roll(w_blizzards, -t, axis=1)
        if working[goal]:
            return t + 1
        t = t + 1


pitchheight, pitchwidth = pitch.shape

for col, ch in enumerate(data[0]):
    if ch == ".":
        start = (0, col - 1)

for col, ch in enumerate(data[len(data) - 1]):
    if ch == ".":
        goal = (pitchheight - 1, col - 1)

# part 1
time1 = find_exit(start, goal, 0)
print(time1)

# # part 2
time2a = find_exit(goal, start, time1)
time2b = find_exit(start, goal, time2a)
print(time2b)
