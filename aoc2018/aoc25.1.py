from __future__ import print_function

import sys
import re
import numpy as np

with open('aoc25.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

pts = [[int(f) for f in re.findall(r'-?\d+', ln)] for ln in data]
pts = np.array(pts)
ptgroups = [set([x]) for x in range(pts.shape[0])]
done = False
while not done:
    done = True
    for i in range(pts.shape[0]-1):
        for j in range(i+1, pts.shape[0]):
            if ptgroups[i] != ptgroups[j]:
                dist = abs(pts[i] - pts[j]).sum()
                if dist <= 3:
                    oldj = ptgroups[j]
                    for jp in oldj:
                        ptgroups[jp] = ptgroups[i]
                        ptgroups[i].add(jp)
                    done = False
    print('loop')

print(len(set(frozenset(p) for p in ptgroups)))
