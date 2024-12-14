from aoc_util import get_data_paras
import numpy as np
import scipy.ndimage
import itertools
import re
import copy

prizes = get_data_paras(13)

total = 0

parsed = []
for rec in prizes:
    m = re.match(
        r"Button A: X\+(\d+), Y\+(\d+)\s*Button B: X\+(\d+), Y\+(\d+)\s*Prize: X=(\d+), Y=(\d+)",
        rec,
    )
    parsed.append(tuple(map(lambda x: int(m.group(x)), [1, 2, 3, 4, 5, 6])))

for parse in parsed:
    (ax, ay, bx, by, px, py) = parse
    cheapest = None
    for apresses in range(101):
        for bpresses in range(101):
            if ax * apresses + bx * bpresses == px and ay * apresses + by * bpresses == py:
                cost = 3 * apresses + bpresses
                if cheapest is None or cost < cheapest:
                    cheapest = cost
    if cheapest is not None:
        total += cheapest
print(total)
newadd = 10000000000000
total = 0
for parse in parsed:
    (ax, ay, bx, by, px, py) = parse
    cheapest = None
    px += newadd
    py += newadd
    # ax*apresses + bx*bpresses == px
    # ay*apresses + by*bpresses == py
    detcoeff = ax * by - ay * bx
    detwob = ax * py - ay * px
    detwoa = bx * py - by * px
    if detwob % detcoeff == 0 and detwoa % detcoeff == 0:
        apresses = -(detwoa // detcoeff)
        bpresses = detwob // detcoeff
        if apresses >= 0 and bpresses >= 0:
            total += 3 * apresses + bpresses

print(total)
