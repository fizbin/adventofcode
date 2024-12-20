from aoc_util import get_data_lines

data = get_data_lines(20)

startloc = None
endloc = None
mymap = {}
for xidx, row in enumerate(data):
    for yidx, ch in enumerate(row):
        mymap[(xidx, yidx)] = ch
        if ch == "S":
            startloc = (xidx, yidx)
        if ch == "E":
            endloc = (xidx, yidx)


def get_neighbors1(spot):
    (myx, myy) = spot
    nbs = [(myx - 1, myy), (myx, myy - 1), (myx, myy + 1), (myx + 1, myy)]
    return [s for s in nbs if mymap.get(s, "$") in ".SE"]


dist_to_end_map = {}
workq = [(endloc, 0)]
while workq:
    w = workq.pop(0)
    if w[0] in dist_to_end_map:
        continue
    dist_to_end_map[w[0]] = w[1]
    for n in get_neighbors1(w[0]):
        workq.append((n, w[1] + 1))

dist_from_start_map = {}
workq = [(startloc, 0)]
while workq:
    w = workq.pop(0)
    if w[0] in dist_from_start_map:
        continue
    dist_from_start_map[w[0]] = w[1]
    for n in get_neighbors1(w[0]):
        workq.append((n, w[1] + 1))

base_dist = dist_to_end_map[startloc]


def get_neighbors2(spot):
    (myx, myy) = spot
    nbs = [(myx - 2, myy), (myx, myy - 2), (myx, myy + 2), (myx + 2, myy)]
    return [s for s in nbs if mymap.get(s, "$") in ".SE"]


goodcheats = set()
for h in dist_from_start_map:
    for n in get_neighbors2(h):
        new_dist = dist_from_start_map[h] + 2 + dist_to_end_map[n]
        if base_dist - new_dist >= 100:
            goodcheats.add((h, n))

print("Part 1:", len(goodcheats))


def get_neighbors20(spot):
    (myx, myy) = spot
    nbs = []
    for xoff in range(-20, 21):
        for yoff in range(-20, 21):
            cheatlen = abs(xoff) + abs(yoff)
            if cheatlen <= 20:
                nbs.append(((myx + xoff, myy + yoff), cheatlen))
    return [s for s in nbs if mymap.get(s[0], "$") in ".SE"]


goodcheats = set()
for h in dist_from_start_map:
    for fn in get_neighbors20(h):
        (n, nd) = fn
        new_dist = dist_from_start_map[h] + nd + dist_to_end_map[n]
        if base_dist - new_dist >= 100:
            goodcheats.add((h, n))

print("Part 2:", len(goodcheats))
