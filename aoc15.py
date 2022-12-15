from aoc_util import *
import numpy as np

data = get_data_lines(15)

sensors = []
for line in data:
    sensors.append(tuple(numbers(line)))

targety = 2000000
excluded = []
beacons = set()
for sensor in sensors:
    mnhatd = abs(sensor[0]-sensor[2]) + abs(sensor[1] - sensor[3])
    d_to_target = abs(sensor[1] - targety)
    span = mnhatd - d_to_target
    if span >= 0:
        range_x = (sensor[0] - span, sensor[0] + span)
        excluded.append(range_x)
    if sensor[3] == targety:
        beacons.add(sensor[2])

excluded.sort()
tot = 0
print(excluded)
currx = excluded[0][0] - 6
for (xmin, xmax) in excluded:
    xmin = max(currx+1, xmin)
    if xmin > xmax:
        continue
    currx = xmax
    tot += (xmax - xmin + 1)
    tot -= len(list(b for b in beacons if xmin <= b <= xmax))
print(tot)