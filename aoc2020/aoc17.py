import sys
import re

def get_all_neighbors(spotset):
    retval = set()
    for (x, y, z) in spotset:
        for xd in (-1, 0, 1):
            for yd in (-1, 0, 1):
                for zd in (-1, 0, 1):
                    retval.add((x+xd, y+yd, z+zd))
    return retval

def get_neighbor_count(spot, spotset):
    retval = 0
    for xd in (-1, 0, 1):
        for yd in (-1, 0, 1):
            for zd in (-1, 0, 1):
                retval += int((spot[0] + xd, spot[1] + yd, spot[2] + zd)
                              in spotset)
    return retval + 99*int(spot in spotset)

def dumpgrid(spotset):
    minz = min(spot[2] for spot in spotset)
    maxz = max(spot[2] for spot in spotset)
    miny = min(spot[1] for spot in spotset)
    maxy = max(spot[1] for spot in spotset)
    minx = min(spot[0] for spot in spotset)
    maxx = max(spot[0] for spot in spotset)
    for z in range(minz, maxz+1):
        print("z =", z, ":")
        for x in range(minx, maxx+1):
            for y in range(miny, maxy+1):
                print('#' if (x, y, z) in spotset else '.', end='')
            print()

def doit():
    with open('aoc17.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)
    spotset = set()
    for i, line in enumerate(data):
        for j, ch in enumerate(line):
            if ch == '#':
                spotset.add((i, j, 0))
    for stage in range(6):
        newspots = set()
        for spot in get_all_neighbors(spotset):
            cnt = get_neighbor_count(spot, spotset)
            if cnt == 102 or cnt == 103 or cnt == 3:
                newspots.add(spot)
        spotset = newspots
        dumpgrid(spotset)
        print()
    print(len(spotset))

if __name__ == '__main__':
    doit()
