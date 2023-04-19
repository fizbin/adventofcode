import sys
import numpy as np

input = 1364
if len(sys.argv) > 2:
    input = int(sys.argv[1])

# for x in range(10):
#     for y in range(10):
#         val = x*x + 3*x + 2*x*y + y + y*y + input
#         if 1 == len(bin(val).replace('b','').replace('0',''))%2:
#             print('\u2588', end='')
#         else:
#             print('.', end='')
#     print()


nsteps = 0
locs = set([(1, 1)])
been_there = set()
while (31, 39) not in locs:
    newlocs = set()
    for x, y in locs:
        if (x, y) in been_there:
            continue
        if x < 0 or y < 0:
            continue
        val = x * x + 3 * x + 2 * x * y + y + y * y + input
        if 1 == len(bin(val).replace("b", "").replace("0", "")) % 2:
            continue
        been_there.add((x, y))
        for xi, yi in [(-1, 0), (0, -1), (1, 0), (0, 1)]:
            newlocs.add((x + xi, y + yi))
    if nsteps == 50:
        p2ans = len(been_there)
    nsteps += 1
    locs = newlocs
    # print(nsteps, locs)

print(nsteps)
print(p2ans)
