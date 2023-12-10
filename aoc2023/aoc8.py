#!/usr/bin/env python

from aoc_util import get_data_paras
import re
import math

data = get_data_paras(8)
lr_list = data[0].strip()
spotmap = {}
for line in data[1].splitlines():
    m = re.match(r"(\w+) = \((\w+), (\w+)\)", line)
    spotmap[m.group(1)] = (m.group(2), m.group(3))

where = "AAA"
steps = 0
while where != "ZZZ":
    todo = lr_list[steps % len(lr_list)]
    if todo == "L":
        where = spotmap[where][0]
    else:
        where = spotmap[where][1]
    steps += 1
print(steps)

fast_forward_map = {}
get_to_z_early_spots = set()
for start in spotmap:
    where = start
    for idx, todo in enumerate(lr_list):
        if idx > 0 and where.endswith("Z"):
            get_to_z_early_spots.add(start)
        if todo == "L":
            where = spotmap[where][0]
        else:
            where = spotmap[where][1]
    fast_forward_map[start] = where

start_network = set(x for x in spotmap if x.endswith("A"))
ssize = 0
while ssize < len(start_network):
    ssize = len(start_network)
    start_network.update(sorted(fast_forward_map[x] for x in start_network))

if get_to_z_early_spots & start_network:
    raise ValueError(
        "Assumptions violated by these start_network places that get to '*z' early:"
        + str(sorted(get_to_z_early_spots & start_network))
    )

factors = []
for start in [x for x in spotmap if x.endswith("A")]:
    where = fast_forward_map[start]
    zjumps = []
    jumps = 1
    beenthere = {start: 0}
    while where not in beenthere:
        beenthere.update({where: jumps})
        if where.endswith("Z"):
            zjumps.append((jumps, where))
        where = fast_forward_map[where]
        jumps += 1
    if beenthere[where] != 1:
        raise ValueError(
            f"Assumptions violated: {start} loops back to restart at {beenthere[where]} at {where}"
        )
    if len(zjumps) != 1 or zjumps[0][0] != jumps - 1:
        raise ValueError(f"Assumptions violated: {zjumps} out of loop found at {jumps}")
    factors.append(jumps - 1)
    print(f"DBG: {start} {zjumps} {where}")

print(len(lr_list) * math.lcm(*factors))
