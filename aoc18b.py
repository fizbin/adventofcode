import sys
import numpy as np
from scipy.ndimage import convolve

# parse input into np.array called lava
datasrc = "aoc18.in" if len(sys.argv) < 2 else sys.argv[1]
data = np.loadtxt(datasrc, delimiter=",", dtype=int)
# in case we had negative coordinates; with this input, it does nothing since data.min() == 0
data -= data.min()
lava = np.zeros((data.max() + 1,) * 3, dtype=int)
lava[tuple(np.transpose(data))] = 1

# This gets us neighbors. ":" means "all the coords" and "None" means "add a new axis"
# So something like np.array([0, 1, 0])[None, :, None] is a 3d box with shape ? x 3 x ?,
# where the ? marks are whatever is needed to make the expression work, and the new
# array has a "1" when the middle coordinate is 1, and a 0 elsewhere.
kernel = (
    np.array([0, 1, 0])[:, None, None]
    + np.array([0, 1, 0])[None, :, None]
    + np.array([0, 1, 0])[None, None, :]
    == 2
).astype(int)

# Kernel is a 3 x 3 x 3 array with:
# [[[0 0 0]
#   [0 1 0]
#   [0 0 0]]

#  [[0 1 0]
#   [1 0 1]
#   [0 1 0]]

#  [[0 0 0]
#   [0 1 0]
#   [0 0 0]]]

# part 1
air = 1 - lava
print((lava * convolve(air, kernel, mode="constant", cval=1)).sum())

# You can view that expression as saying: "push each air cell up, down, north, south, east, west,
# then keep only the air totals pushed into spots with lava, and sum them"
# The mode="constant", cval=1 bit says to treat the edges as though there were "1" values
# (i.e. more air) surrounding them.

# part 2
air = lava * 0
oldair = air + 1
while (air != oldair).any():
    oldair[...] = air
    air = (oldair + (1 - lava) * convolve(oldair, kernel, mode="constant", cval=1))

    # this just turns any non-zero value into 1
    air = air.astype(bool).astype(int)

# note that this is the same expression as part 1 
print((lava * convolve(air, kernel, mode="constant", cval=1)).sum())
