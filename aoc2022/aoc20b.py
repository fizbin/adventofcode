from aoc_util import *
import copy
import itertools
from functools import reduce
import re
import heapq


data = numbers(get_data(20))

# data = numbers("""
# 1
# 2
# -3
# 3
# -2
# 0
# 4
# """)

order = list(range(len(data)))

for (idx, val) in enumerate(data):
    # print(idx, val, len(set(order)))
    curridx = order[idx]
    rval = val % (len(data) - 1)
    secondidx = curridx + rval
    if secondidx >= len(data):
        secondidx = curridx - (len(data) - 1) + rval
    #secondidx = (curridx + (val % (len(data) - 1))) % len(data)
    neworder = list(order)
    if secondidx < curridx:
        neworder = [i+1 if secondidx <= i < curridx else i for i in neworder]
        neworder[idx] = secondidx
        # print("<", idx, val, curridx, secondidx, len(set(neworder)))
    elif curridx < secondidx:
        neworder = [i-1 if curridx < i <= secondidx else i for i in neworder]
        neworder[idx] = secondidx
        # print("<", idx, val, curridx, secondidx, len(set(neworder)))
    order = neworder
    assert len(set(order)) == len(data)
    #print("order", order)
    tvals = [None] * len(data)
    for (idx, ord) in enumerate(order):
        tvals[ord] = data[idx]
    #print("tvals", tvals)


orderinv = [None] * len(data)
for (idx, val) in enumerate(order):
    orderinv[val] = idx

# print(order)

# print(orderinv)

zeroidx = data.index(0)
zerospot = order[zeroidx]
tot = 0
for off in (1000, 2000, 3000):
    sp = (off + zerospot) % len(data)
    # print(data[orderinv[sp]])
    tot += data[orderinv[sp]]
print(tot)


order = list(range(len(data)))

for _ in range(10):
    for (idx, val) in enumerate(data):
        # print(idx, val, len(set(order)))
        curridx = order[idx]
        rval = (val*811589153) % (len(data) - 1)
        secondidx = curridx + rval
        if secondidx >= len(data):
            secondidx = curridx - (len(data) - 1) + rval
        #secondidx = (curridx + (val % (len(data) - 1))) % len(data)
        neworder = list(order)
        if secondidx < curridx:
            neworder = [i+1 if secondidx <= i < curridx else i for i in neworder]
            neworder[idx] = secondidx
            # print("<", idx, val, curridx, secondidx, len(set(neworder)))
        elif curridx < secondidx:
            neworder = [i-1 if curridx < i <= secondidx else i for i in neworder]
            neworder[idx] = secondidx
            # print("<", idx, val, curridx, secondidx, len(set(neworder)))
        order = neworder
        assert len(set(order)) == len(data)
        #print("order", order)
        tvals = [None] * len(data)
        for (idx, ord) in enumerate(order):
            tvals[ord] = data[idx]
        #print("tvals", tvals)


orderinv = [None] * len(data)
for (idx, val) in enumerate(order):
    orderinv[val] = idx

# print(order)

# print(orderinv)

zeroidx = data.index(0)
zerospot = order[zeroidx]
tot = 0
for off in (1000, 2000, 3000):
    sp = (off + zerospot) % len(data)
    # print(data[orderinv[sp]])
    tot += 811589153*data[orderinv[sp]]
print(tot)
