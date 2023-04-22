import sys
import re
import heapq

infile = "aoc24.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = [x.strip() for x in infilep]

alldigits_builder = set()
startspot = None
for ridx, line in enumerate(data):
    for cidx, char in enumerate(line):
        if char == "0":
            startspot = (ridx, cidx)
        if char.isdigit():
            alldigits_builder.add(char)

alldigits = frozenset(alldigits_builder)
been_there = set()
boundary = [(startspot, frozenset(["0"]))]
nsteps = 0
while True:
    oboundary = boundary
    boundary = []
    for (r, c), has in oboundary:
        if data[r][c] == ".":
            pass
        elif data[r][c] == "#":
            continue
        else:
            has = has | frozenset([data[r][c]])
        if has == alldigits:
            break
        if ((r, c), has) in been_there:
            continue
        been_there.add(((r, c), has))
        boundary.append(((r + 1, c), has))
        boundary.append(((r - 1, c), has))
        boundary.append(((r, c + 1), has))
        boundary.append(((r, c - 1), has))
    else:
        nsteps += 1
        continue
    break

print(nsteps)

# part 2

been_there = set()
boundary = [(startspot, frozenset(["0"]))]
nsteps = 0
while True:
    oboundary = boundary
    boundary = []
    for (r, c), has in oboundary:
        if data[r][c] == ".":
            pass
        elif data[r][c] == "#":
            continue
        else:
            has = has | frozenset([data[r][c]])
        if has == alldigits:
            break
        has = has - frozenset("0")
        if ((r, c), has) in been_there:
            continue
        been_there.add(((r, c), has))
        boundary.append(((r + 1, c), has))
        boundary.append(((r - 1, c), has))
        boundary.append(((r, c + 1), has))
        boundary.append(((r, c - 1), has))
    else:
        nsteps += 1
        continue
    break

print(nsteps)
