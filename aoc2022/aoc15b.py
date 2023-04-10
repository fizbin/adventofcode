from functools import reduce
from aoc_util import numbers, get_data_lines

data = get_data_lines(15)

sensors = []
maxval = 0
for line in data:
    (a, b, c, d) = numbers(line)
    sensors.append(((a,b),(c,d)))
    maxval = max(maxval, a, b, c, d)

def subtract_range(ranges, subtrahend):
    retval = []
    for minuend in ranges:
        ((a, b), (c, d)) = (minuend, subtrahend)
        if a > d or b < c:  # disjoint
            retval.append(minuend)
        elif c <= a <= b <= d:  # completely gone
            pass
        elif c <= a <= d < b:
            retval.append((d+1, b))
        elif a < c <= b <= d:
            retval.append((a, c-1))
        elif a < c <= d < b:
            retval.append((a, c-1))
            retval.append((d+1, b))
    return retval

def getExclusion(y, sensor, beacon):
    bdist = abs(sensor[0] - beacon[0]) + abs(sensor[1] - beacon[1])
    rdist = bdist - abs(sensor[1] - y)
    if rdist < 0:
        return None
    return (sensor[0] - rdist, sensor[0] + rdist)

p1Range = [(-3*maxval, 3*maxval-1)]
twomill = 2000000
for (sensor, beacon) in sensors:
    excl = getExclusion(twomill, sensor, beacon)
    if excl is not None:
        p1Range = subtract_range(p1Range, excl)
nbeacons = len(set([b for (_, b) in sensors if b[1] == twomill]))
left_size = sum(b - a + 1 for (a, b) in p1Range)
print(6*maxval - left_size + nbeacons)

fourmill = 2 * twomill
for y in range(fourmill + 1):
    p2Range = [(0, fourmill)]
    for (sensor, beacon) in sensors:
        excl = getExclusion(y, sensor, beacon)
        if excl is not None:
            p2Range = subtract_range(p2Range, excl)
    if p2Range:
        xval = p2Range[0][0]
        print (y + fourmill*xval)
        break
