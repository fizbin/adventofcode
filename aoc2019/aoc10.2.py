#!/usr/bin/env python
import sys
import cmath

with open('aoc10.in' if len(sys.argv) < 4 else sys.argv[3]) as f:
    data = list(f)

asters = set()
for (row, line) in enumerate(data):
    for (col, char) in enumerate(line):
        if char == '#':
            asters.add(row + 1j*col)


def visfrom(sta):
    spotfoo = {}
    for aster in asters:
        zcoord = (aster - sta)*(1-0.0000001j)
        if abs(zcoord) > 0:
            newnormed = round(cmath.phase(zcoord)*65536)/65536
            spotfoo.setdefault(newnormed, []).append(aster)

    for phase in spotfoo:
        spotfoo[phase] = sorted(
            spotfoo[phase],
            key=lambda z: -abs(z-sta))

    return spotfoo


(p1ans, _, beststa) = max((len(visfrom(sta)), str(sta), sta) for sta in asters)
print("Part 1: %s" % (p1ans,))
print("Best station: %s" % (beststa,))

thing = visfrom(beststa)
vaporized = 0
foundone = True
while foundone:
    foundone = False
    for phase in sorted(thing, reverse=True):
        if thing[phase]:
            lastwiped = thing[phase].pop()
            vaporized += 1
            if vaporized == 200:  # or True:  # for debugging
                print("Vaporized %3d at %8s (== %s) (@ %s)" % (
                    vaporized, lastwiped,
                    int(100*(lastwiped.imag) + lastwiped.real), phase))
            foundone = True
