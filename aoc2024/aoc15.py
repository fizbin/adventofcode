from aoc_util import get_data_lines, get_data, get_data_paras
import numpy as np
import scipy.ndimage
import itertools
import re
import copy

mymapspec, mymomves = get_data_paras(15)

rspot = None
mymap = {}
for xidx, row in enumerate(mymapspec.splitlines()):
    for yidx, ch in enumerate(row):
        mymap[xidx + 1j * yidx] = ch
        if ch == "@":
            rspot = xidx + 1j * yidx


def dumpmap():
    was = 0 + 0j
    for where, what in mymap.items():
        if where.real != was.real:
            print()
        was = where
        print(what, end="")
    print()
    print()


rdirs = {"^": -1, ">": 1j, "v": 1, "<": -1j}

for move in "".join(mymomves.splitlines()):
    rdir = rdirs[move]
    if mymap[rspot + rdir] == "#":
        continue
    if mymap[rspot + rdir] == ".":
        mymap[rspot] = "."
        rspot += rdir
        mymap[rspot] = "@"
        continue
    endmove = rspot + rdir
    while mymap.get(endmove, "#") == "O":
        endmove += rdir
    if mymap[endmove] == "#":
        continue
    bkwd = endmove
    while bkwd != rspot:
        mymap[bkwd] = mymap[bkwd - rdir]
        bkwd -= rdir
    mymap[rspot] = "."
    rspot += rdir

# print(mymap)

total = 0
for loc, thing in mymap.items():
    if thing == "O":
        total += 100 * int(loc.real) + int(loc.imag)
print("Part 1:", total)


rspot = None
mymap = {}
for xidx, row in enumerate(mymapspec.splitlines()):
    for yidx, ch in enumerate(row):
        if ch in ".#":
            mymap[xidx + 2j * yidx] = ch
            mymap[xidx + 2j * yidx + 1j] = ch
        if ch == "O":
            mymap[xidx + 2j * yidx] = "["
            mymap[xidx + 2j * yidx + 1j] = "]"
        if ch == "@":
            rspot = xidx + 2j * yidx
            mymap[xidx + 2j * yidx] = ch
            mymap[xidx + 2j * yidx + 1j] = "."


for move in "".join(mymomves.splitlines()):
    for where, what in mymap.items():
        if what == "@" and where != rspot:
            raise Exception("Robot duplicate")
    rdir = rdirs[move]
    if mymap[rspot + rdir] == "#":
        continue
    if mymap[rspot + rdir] == ".":
        mymap[rspot] = "."
        rspot += rdir
        mymap[rspot] = "@"
        continue
    # Okay, moving a box chain
    if rdir.real == 0.0:
        # same as before, left-right:
        endmove = rspot + rdir
        while mymap.get(endmove, "#") in "[]":
            endmove += rdir
        if mymap[endmove] == "#":
            continue
        bkwd = endmove
        while bkwd != rspot:
            mymap[bkwd] = mymap[bkwd - rdir]
            bkwd -= rdir
        mymap[rspot] = "."
        rspot += rdir
    else:
        if mymap[rspot + rdir] == "[":
            pushing = [rspot + rdir, rspot + rdir + 1j]
        elif mymap[rspot + rdir] == "]":
            pushing = [rspot + rdir, rspot + rdir - 1j]
        else:
            raise (Exception("WTF? " + repr(mymap[rspot + rdir])))
        pushed = list(pushing) + [rspot]
        abortmove = False
        while pushing:
            nextpushing = [x + rdir for x in pushing]
            if any(mymap[np] == "#" for np in nextpushing):
                abortmove = True
                break
            pushing = [x for x in nextpushing if mymap[x] in "[]"]
            extra = []
            for spot in pushing:
                if mymap[spot] == "[" and spot + 1j not in pushing:
                    extra.append(spot + 1j)
                if mymap[spot] == "]" and spot - 1j not in pushing:
                    extra.append(spot - 1j)
            pushing += extra
            pushed += pushing
        if abortmove:
            continue
        newspots = {}
        for x in pushed:
            newspots[x] = "."
        for x in pushed:
            newspots[x + rdir] = mymap[x]
        mymap.update(newspots)
        rspot += rdir

total = 0
for loc, thing in mymap.items():
    if thing == "[":
        total += 100 * int(loc.real) + int(loc.imag)
print("Part 2:", total)
