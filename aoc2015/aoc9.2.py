import sys
import re
from collections import defaultdict
import itertools

with open('aoc9.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

dists = defaultdict(dict)

distsum = 0
for ln in data:
    m = re.match(r'(\w+) to (\w+) = (\d+)', ln)
    (von, zu, d_s) = m.groups()
    d = int(d_s)
    dists[von][zu] = d
    dists[zu][von] = d
    distsum += d

allspots = list(dists)
allspots.sort()

mindist = 0
for chain in itertools.permutations(allspots[1:]):
    spotlist = (allspots[0],) + chain + (allspots[0],)
    m_in_chain = distsum
    tot = 0
    for i in range(1 + len(chain)):
        m_in_chain = min(m_in_chain, dists[spotlist[i]][spotlist[i+1]])
        tot += dists[spotlist[i]][spotlist[i+1]]
    mindist = max(mindist, tot - m_in_chain)

print(mindist)
