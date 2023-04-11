import sys
import functools
import operator
import numpy as np


def knot_hash(raw_data):
    data = [ord(x) for x in raw_data] + [17, 31, 73, 47, 23]
    mylist = list(range(256))
    skip = 0
    cp = 0
    for _ in range(64):
        for length in data:
            listy = mylist[cp:] + mylist[:cp]
            listy[0:length] = list(reversed(listy[0:length]))
            mylist = listy[len(listy) - cp :] + listy[: len(listy) - cp]
            cp += length + skip
            cp %= len(mylist)
            skip += 1

    hash = ""
    for start in range(0, 256, 16):
        val = functools.reduce(operator.xor, mylist[start : start + 16], 0)
        hash += "%02x" % (val,)
    return hash


myinput = "ffayrhll"
if len(sys.argv) > 1:
    myinput = sys.argv[1]

numpy_pre = []
for row in range(128):
    hash = knot_hash(f"{myinput}-{row}")
    row = []
    while hash:
        binchunk = bin((1 << 32) + int(hash[:8], 16))[3:]
        hash = hash[8:]
        row.extend(int(x) for x in binchunk)
    numpy_pre.append(row)

np_array = np.array(numpy_pre, dtype=int)
print(np_array.sum())

equiv_core = (1 + np.arange(128 * 128).reshape((128, 128))) * np_array
import pprint

equiv = np.zeros((130, 130), dtype=int)
equiv[1:129, 1:129] = equiv_core
old_equiv = np.zeros_like(equiv)
kernel = np.zeros_like(equiv)
kernel[1:129, 1:129] = np_array
while (equiv != old_equiv).any():
    old_equiv[...] = equiv
    equiv = np.maximum(equiv, np.roll(old_equiv, 1, 0)) * kernel
    equiv = np.maximum(equiv, np.roll(old_equiv, -1, 0)) * kernel
    equiv = np.maximum(equiv, np.roll(old_equiv, 1, 1)) * kernel
    equiv = np.maximum(equiv, np.roll(old_equiv, -1, 1)) * kernel

print(len(set(equiv.flatten())) - 1)
