#!/usr/bin/env python3
import sys
import re
import collections

with open('aoc9.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = [int(x) for x in list(f)]

def acceptable(nval, prevvals):
    sums = set()
    for pval in prevvals:
        if (nval - pval) in sums:
            return True
        sums.add(pval)
    return False

p_len = 25
goal = None
for (idx, nval) in enumerate(data[p_len:]):
    if not acceptable(nval, data[idx:idx + p_len]):
        print(nval)
        goal = nval
        break

for idx1 in range(len(data)):
    for idx2 in range(idx1+2, len(data)):
        if goal == sum(data[idx1:idx2]):
            print(min(data[idx1:idx2]) + max(data[idx1:idx2]))
            break
    else:
        continue
    break
