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

def covers_square(tl, br, sensor):
    bdist = manhat(sensor[0:2], sensor[2:])
    sensor_real = sensor[0:2]
    tr = (tl[0], br[1])
    bl = (br[0], tl[1])
    return (manhat(sensor_real, tl) <= bdist
           and manhat(sensor_real, br) <= bdist
           and manhat(sensor_real, tr) <= bdist
           and manhat(sensor_real, bl) <= bdist)

def divide_square(tl, br):
    xdist = br[0] - tl[0] + 1
    ydist = br[1] - tl[1] + 1
    if xdist > 1 and ydist > 1:
        xdiv = tl[0] + xdist // 2
        ydiv = tl[1] + ydist // 2
        return [(tl, (xdiv - 1, ydiv - 1)),
                ((xdiv,tl[1]), (br[0], ydiv - 1)),
                ((tl[0], ydiv),( xdiv -1, br[1])),
                ((xdiv,ydiv), br)]
    if xdist > 1:
        xdiv = tl[0] + xdist // 2
        return [(tl, (xdiv - 1, br[1])),
                ((xdiv, tl[1]), br)]
    if ydist > 1:
        ydiv = tl[1] + ydist // 2
        return [(tl, (tl[0], ydiv - 1)),
                ((tl[0], ydiv), br)]
    return [(tl,br)]

def squaresize(sq):
    (tl, br) = sq
    ret = (br[0]-tl[0]+1)*(br[1]-tl[1]+1)
    if ret < 0:
        print (f"Bad square {sq}")
    return ret

squares = [((0,0),(4000000,4000000))]
while len(squares) > 1 or squaresize(squares[0]) > 1:
    nsquares = []
    for sq in squares:
        if not any(covers_square(sq[0],sq[1], sensor) for sensor in sensors):
            xtnd = divide_square(sq[0], sq[1])
            #print(f"{sq} became {xtnd}")
            nsquares.extend(divide_square(sq[0], sq[1]))
    squares = nsquares

print(squares[0][0][0]*4000000 + squares[0][0][1])