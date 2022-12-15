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


# part 2

def manhat(pt1, pt2):
    return abs(pt1[0] - pt2[0]) + abs(pt1[1] - pt2[1])

def convert_sensor(sensor):
    """
    Output is in (x+y, x-y)=(u,v) space. Output is (umin,umax,vmin,vmax)
    """
    (sx, sy, bx, by) = sensor
    bdist = manhat((sx,sy),(bx,by))
    return (sx+sy-bdist,sx+sy+bdist,sx-sy-bdist,sx-sy+bdist)

def subtract_rect(rects, subtrahend):
    retval = []
    (umin, umax, vmin, vmax) = subtrahend
    working = list(rects)
    while working:
        rect = working.pop()
        (rumin, rumax, rvmin, rvmax) = rect
        if (rumax < umin or umax < rumin or rvmax < vmin or vmax < rvmin):
            # disjoint
            retval.append(rect)
        elif (umin <= rumin <= rumax <= umax and vmin <= rvmin <= rvmax <= vmax):
            pass # consumed
        elif (rumin < umin <= rumax):
            working.append((rumin, umin-1, rvmin, rvmax))
            working.append((umin, rumax, rvmin, rvmax))
        elif (rumin <= umax < rumax):
            working.append((rumin, umax, rvmin, rvmax))
            working.append((umax+1, rumax, rvmin, rvmax))
        elif (rvmin < vmin <= rvmax):
            working.append((rumin, rumax, rvmin, vmin-1))
            working.append((rumin, rumax, vmin, rvmax))
        elif (rvmin <= vmax < rvmax):
            working.append((rumin, rumax, rvmin, vmax))
            working.append((rumin, rumax, vmax+1, rvmax))
    return retval

converted = [convert_sensor(s) for s in sensors]

regions = [(0, 8000000, -4000000, 4000000)]
for cvt in converted:
    regions = subtract_rect(regions, cvt)

singletons = [r for r in regions if r[0]==r[1] and r[2]==r[3]]
csing = [((r[0]+r[2])//2, (r[0]-r[2])//2) for r in singletons]
csing = [r for r in csing if 0 <= r[0] <= 4000000 and 0 <= r[1] <= 4000000]
assert len(csing) == 1
print(csing[0][0]*4000000+csing[0][1])