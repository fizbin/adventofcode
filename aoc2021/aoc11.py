"""
Advent of code day 11 in python.
"""

import sys
import re
import collections
import itertools
import copy

with open("aoc11.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
    # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    data = re.findall(r"\S+", f.read())
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

octos = [[int(c) for c in line] for line in data]


def dostep(octs):
    newval = copy.deepcopy(octs)
    for i in range(10):
        for j in range(10):
            #            print(newval, i, j)
            newval[i][j] += 1
    keep_going = True
    while keep_going:
        keep_going = False
        for i in range(10):
            for j in range(10):
                if newval[i][j] > 9 and newval[i][j] < 1000:
                    newval[i][j] += 1000
                    keep_going = True
                    for idi in (-1, 0, 1):
                        for jdi in (-1, 0, 1):
                            if idi + i in range(10) and jdi + j in range(10):
                                newval[idi + i][jdi + j] += 1
    count = 0
    for i in range(10):
        for j in range(10):
            if newval[i][j] >= 1000:
                count += 1
                newval[i][j] = 0
    return (count, newval)


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
