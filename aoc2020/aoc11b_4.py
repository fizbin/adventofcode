#!/usr/bin/env python3
import sys
import random
import re
import functools

with open('aoc11.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

diagram = dict( (r + c*1j, occ=='#')
                for (r, line) in enumerate(data)
                for (c, occ) in enumerate(line)
                if (occ != '.'))

@functools.lru_cache(maxsize=None)
def nbhd(z):
    result = []
    for zdir in (1, -1, 1j, -1j, 1+1j, 1-1j, -1+1j, -1-1j):
        for mul in range(1, len(diagram)+2):
            if z + mul*zdir in diagram:
                result.append(z+mul*zdir)
                break
    return result

def surrounding(z):
    result = 0
    for z1 in nbhd(z):
        result += int(diagram[z1])
    return result

done = False
new_d = diagram
while not done:
    diagram = new_d
    new_d = {}
    for z, zocc in diagram.items():
        if zocc:
            new_d[z] = surrounding(z) < 5
        else:
            new_d[z] = surrounding(z) == 0
    done = (''.join(sorted(repr(it) for it in new_d.items())) ==
            ''.join(sorted(repr(it) for it in diagram.items())))

#print('\n'.join(diagram))
print(len([z for z in diagram if diagram[z]]))
