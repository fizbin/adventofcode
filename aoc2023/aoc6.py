#!/usr/bin/env python

from aoc_util import get_data_lines, numbers
import re

data = get_data_lines(6)
times = numbers(data[0])
dists = numbers(data[1])
total = 1
for t, d in zip(times, dists):
    myways = 0
    for h in range(t + 1):
        if h * (t - h) > d:
            myways += 1
    total *= myways
print(total)

(bigtime,) = numbers(data[0].replace(" ", ""))
(bigdist,) = numbers(data[1].replace(" ", ""))
minh1 = 0
minh2 = bigtime // 2 - 1
while minh2 - minh1 > 1:
    mid = (minh1 + minh2) // 2
    if mid * (bigtime - mid) > bigdist:
        minh2 = mid
    else:
        minh1 = mid
maxh1 = bigtime // 2 + 1
maxh2 = bigtime
while maxh2 - maxh1 > 1:
    mid = (maxh1 + maxh2) // 2
    if mid * (bigtime - mid) > bigdist:
        maxh1 = mid
    else:
        maxh2 = mid
# print(minh1,minh2,maxh1,maxh2)
print(maxh2 - minh2)
