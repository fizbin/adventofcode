from __future__ import print_function

import sys
import collections

with open('aoc20.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read().strip()

if data[0] != '^':
    raise Exception("Didn't start with ^")
if data[-1] != '$':
    raise Exception("Didn't end with $")

global_directions = data[1:-1]

dirs = {'N': 0+1j, 'S': 0-1j, 'E': 1+0j, 'W': -1+0j}
graph = collections.defaultdict(set)

def parse(startset, stuff):
    locset = set(startset)
    tot_locset = set()
    while stuff:
        now = stuff.pop(0)
        if now == '(':
            locset = parse(locset, stuff)
        elif now == '|':
            tot_locset.update(locset)
            locset = set(startset)
        elif now == ')':
            tot_locset.update(locset)
            return tot_locset
        elif now in 'NSEW':
            move = dirs[now]
            new_locset = set([])
            for loc in locset:
                nloc = loc + move
                graph[loc].add(nloc)
                graph[nloc].add(loc)
                new_locset.add(nloc)
            locset = new_locset


parse(set([0+0j]), list(global_directions))

dists = {0+0j: 0}
working = collections.deque([0+0j])
while working:
    now = working.popleft()
    for nloc in graph[now]:
        if nloc not in dists or dists[nloc] > dists[now] + 1:
            working.append(nloc)
            dists[nloc] = dists[now] + 1

print(max(dists.values()))

print(len([x for (x, d) in dists.items() if d >= 1000]))

print(max(z.real for z in dists),
      max(z.imag for z in dists),
      min(z.real for z in dists),
      min(z.imag for z in dists))
# W(N|S)E
# ((0, 0), 0) []
#    W
# ((-1, 0), 1) []
#    (
# ((-1, 0), 2) [((-1, 0), 2)]
#    N
# ((-1, 1), 3) [((-1, 0), 2)]
#    |
#
