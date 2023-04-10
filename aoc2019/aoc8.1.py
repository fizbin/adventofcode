#!/usr/bin/python
import sys
import numpy as np
import re

with open('aoc8.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

data = [int(x) for x in re.findall(r'\d', data[0])]

arr = np.reshape(data, (len(data) // 150, 6, 25))

arr0count = (arr == 0).sum(axis=(1, 2))
spot = np.where(arr0count == np.amin(arr0count))
arr1count = (arr == 1).sum(axis=(1, 2))
arr2count = (arr == 2).sum(axis=(1, 2))

print(arr1count[spot] * arr2count[spot])

img = 2 * np.ones((6, 25), dtype=int)
for layer in range(arr.shape[0]):
    layerdata = arr[layer]
    img = (img < 2) * img + (img >= 2) * layerdata

print(img)
