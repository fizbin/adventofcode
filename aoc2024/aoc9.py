from aoc_util import get_data_paras, get_data
import re
import collections
import copy
import itertools


spec = get_data(9).strip()
# spec = '2333133121414131402'

dirmap = []
isfile = True
filenum = 0
for dig in spec:
    if isfile:
        dirmap.extend([filenum] * int(dig))
        filenum += 1
        isfile = False
    else:
        dirmap.extend([-1] * int(dig))
        isfile = True

lftptr = 0
rgtptr = len(dirmap) - 1
while lftptr < rgtptr:
    while dirmap[lftptr] != -1 and lftptr < rgtptr:
        lftptr += 1
    while dirmap[rgtptr] == -1 and lftptr < rgtptr:
        rgtptr -= 1
    while dirmap[lftptr] == -1 and dirmap[rgtptr] != -1 and lftptr < rgtptr:
        dirmap[lftptr] = dirmap[rgtptr]
        dirmap[rgtptr] = -1
        lftptr += 1
        rgtptr -= 1

total = 0
for idx, val in enumerate(dirmap):
    if val != -1:
        total += idx * val
print(total)

filespots = []
freespots = []
isfile = True
filenum = 0
loc = 0
for dig in spec:
    if isfile:
        filespots.append((loc, int(dig), filenum))
        filenum += 1
        isfile = False
        loc += int(dig)
    else:
        freespots.append((loc, int(dig), -1))
        isfile = True
        loc += int(dig)

for fidx, (fileloc, filelen, filenum) in reversed(list(enumerate(filespots))):
    for freeidx, (freeloc, freelen, _) in enumerate(freespots):
        if freelen >= filelen and freeloc < fileloc:
            freespots[freeidx] = (freeloc + filelen, freelen - filelen, -1)
            filespots[fidx] = (freeloc, filelen, filenum)
            if freelen == filelen:
                del freespots[freeidx]
            break
        if freeloc > fileloc:
            break

dirmap2 = [-1] * len(dirmap)

for fileloc, filelen, filenum in filespots:
    assert all(x == -1 for x in dirmap2[fileloc : fileloc + filelen])
    dirmap2[fileloc : fileloc + filelen] = [filenum] * filelen

total = 0
for idx, val in enumerate(dirmap2):
    if val != -1:
        total += idx * val
print(total)
