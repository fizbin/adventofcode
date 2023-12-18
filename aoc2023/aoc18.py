#!/usr/bin/python

import aoc_util

digplan = aoc_util.get_data_lines(18)
digcoords = {}
curcord = (0,0)
minxcord = 0
dugcords = set()
for (dirtxt, lentxt, _) in [ln.split(" ", 2) for ln in digplan]:
    diglen = int(lentxt)
    digdir = (0,0)
    if dirtxt == 'U':
        digdir = (-1,0)
    elif dirtxt == 'D':
        digdir = (1,0)
    elif dirtxt == 'L':
        digdir = (0,-1)
    elif dirtxt == 'R':
        digdir = (0,1)
    stopcord = (curcord[0] + digdir[0]*diglen, curcord[1] + digdir[1]*diglen)
    minxcord = min(minxcord, stopcord[0])
    for l in range(diglen+1):
        dugcords.add((curcord[0] + digdir[0]*l, curcord[1] + digdir[1]*l))
    curcord = stopcord
# print(f"min is {minxcord}")
for (dirtxt, lentxt, _) in [ln.split(" ", 2) for ln in digplan]:
    diglen = int(lentxt)
    digdir = (0,0)
    if dirtxt == 'U':
        digdir = (-1,0)
    elif dirtxt == 'D':
        digdir = (1,0)
    elif dirtxt == 'L':
        digdir = (0,-1)
    elif dirtxt == 'R':
        digdir = (0,1)
    stopcord = (curcord[0] + digdir[0]*diglen, curcord[1] + digdir[1]*diglen)
    if digdir == (0,1):
        for y in range(curcord[1], curcord[1] + diglen):
            for x in range(minxcord - 1, curcord[0]+1):
                digcoords[x,y] = digcoords.get((x,y),0) + 1
    if digdir == (0,-1):
        for y in range(curcord[1] - diglen, curcord[1]):
            for x in range(minxcord - 1, curcord[0]):
                digcoords[x,y] = digcoords.get((x,y),0) - 1
    curcord = stopcord

intcords = set([x for x in digcoords if digcoords[x] != 0]) | dugcords

print(len(intcords))

perilen = 0
curcord = (0,0)
twoarea = 0
for (_1, _2, enc) in [ln.split(" ", 2) for ln in digplan]:
    diglen = int(enc[2:7], 16)
    dirtxt = enc[7:8]
    if dirtxt == '3':
        digdir = (-1,0)
    elif dirtxt == '1':
        digdir = (1,0)
    elif dirtxt == '2':
        digdir = (0,-1)
    elif dirtxt == '0':
        digdir = (0,1)
    perilen += diglen
    stopcord = (curcord[0] + digdir[0]*diglen, curcord[1] + digdir[1]*diglen)
    twoarea += curcord[0] * stopcord[1] - curcord[1] * stopcord[0]
    curcord = stopcord

computed_area = abs(twoarea / 2)
full_area = computed_area + (perilen - 4) / 2 + 3
print(int(full_area))
