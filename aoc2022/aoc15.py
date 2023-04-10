from aoc_util import numbers, get_data_lines

data = get_data_lines(15)

sensors = []
for line in data:
    sensors.append(tuple(numbers(line)))

def find_excluded(targety):
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
    return (excluded, beacons)

(excluded, beacons) = find_excluded(2000000)
tot = 0
currx = excluded[0][0] - 6
for (xmin, xmax) in excluded:
    xmin = max(currx+1, xmin)
    if xmin > xmax:
        continue
    currx = xmax
    tot += (xmax - xmin + 1)
    tot -= len(list(b for b in beacons if xmin <= b <= xmax))
print(tot)

for y in range(4000000):
    (excluded, _) = find_excluded(y)
    currx = -1
    tot = 0
    used_ex = []
    for (xmin, xmax) in excluded:
        xmin = max(currx+1, xmin)
        xmax = min(xmax, 4000000)
        if xmin > xmax:
            continue
        currx = xmax
        tot += (xmax - xmin + 1)
        used_ex.append((xmin,xmax))
        if xmax == 4000000:
            break
    if tot < 4000001:
        # print (y, tot, used_ex)
        for ((a, b),(c,d)) in zip(used_ex, used_ex[1:]):
            if b < c - 1:
                x = c-1
        print(y + 4000000*x)
        break
