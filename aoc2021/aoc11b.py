"""
Advent of code day 11 in python.
"""

import sys
import re
import numpy as np

if __name__ == '__main__':
    with open("aoc11.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
        data = re.findall(r"\S+", f.read())
        # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
        # data = [tuple(int(y) for y in tup) for tup in data]

    octos = np.array([[int(c) for c in line] for line in data], dtype=int)

def spread(boolarr):
    bshape = np.shape(boolarr)
    retval = np.zeros_like(boolarr, dtype=int)
    for xdir in [(0,-1,1,bshape[0]), (0,bshape[0],0,bshape[0]), (1,bshape[0],0,-1)]:
        for ydir in [(0,-1,1,bshape[1]), (0,bshape[1],0,bshape[1]), (1,bshape[1],0,-1)]:
            retval[xdir[0]:xdir[1],ydir[0]:ydir[1]] += boolarr[xdir[2]:xdir[3],ydir[2]:ydir[3]].astype(int)
    return retval


def dostep(octs):
    newval = octs + 1

    newval2 = newval + spread(newval >= 10)
    while((newval2 != newval).any()):
        newval[:] = newval2
        newval2[:] = octs + 1 + spread(newval >= 10)
    
    count = (newval >= 10).astype(int).sum()
    newval = newval * (newval < 10).astype(int)
    return (count, newval)

if __name__ == '__main__':

    tot = 0
    myocts = octos
    for _ in range(100):
        (flashers, myocts) = dostep(myocts)
        tot += flashers
    print(tot)

    myocts = octos
    for gen in range(1, 999999):
        (flashers, myocts) = dostep(myocts)
        if flashers == 100:
            print(gen)
            break
