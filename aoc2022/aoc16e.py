from aoc_util import get_data_lines
import re
import heapq
from typing import Dict, Tuple
import itertools
import numpy as np

data = get_data_lines(16)

valvemap = {}
valverates = {}

for line in data:
    m = re.match(
        r"Valve (\S+) has flow rate=(\d+); tunnels? leads? to valves? (.*)", line
    )
    if m is None:
        print("ERR: " + line)
    name = m.group(1)
    rate = int(m.group(2))
    rest = [dest.strip() for dest in m.group(3).split(",")]
    valvemap[name] = tuple(rest)
    valverates[name] = rate

shortest: Dict[Tuple[str, str], int] = {}
for (waypoint, nexts) in valvemap.items():
    shortest[(waypoint, waypoint)] = 0
    for dest in nexts:
        shortest[(waypoint, dest)] = 1
for waypoint in valvemap:
    for first in valvemap:
        for last in valvemap:
            if first != last:
                shortest[(first, last)] = min(
                    shortest.get((first, last), 999),
                    shortest.get((first, waypoint), 999)
                    + shortest.get((waypoint, last), 999),
                )

to_int = dict(
    zip([v for v in valverates if valverates[v] > 0] + ["AA"], range(len(valvemap)))
)
aa_int = to_int["AA"]

distmat = np.ndarray((len(to_int), len(to_int)), dtype=int)
ratevec = np.ndarray((len(to_int),), dtype=int)
for (froms, fromi) in to_int.items():
    ratevec[fromi] = valverates[froms]
    for (tos, toi) in to_int.items():
        distmat[fromi, toi] = shortest[(froms, tos)]
    distmat[fromi, fromi] = 1
    distmat[fromi, aa_int] = 999

ratelookup = np.zeros((1,), dtype=int)
for idx in range(aa_int):
    ratelookup = np.append(ratelookup, ratelookup + ratevec[idx])


sum_of_valverates = ratevec.sum()
# Things in all_spots are: (sort key, pressure released so far, location, open valves, time, old_time)
all_spots = [(0, 0, aa_int, 0, 0, 0)]
best_so_far = -1
best_to_26 = {}
been_here = {}
sample = 0
max_len_all_spots = 0
while all_spots:
    (_, released_so_far, loc, open_valves, time, otime) = heapq.heappop(all_spots)
    sample += 1
    max_len_all_spots = max(max_len_all_spots, len(all_spots))
    if been_here.get((loc, open_valves, time), -1) >= released_so_far:
        continue
    been_here[(loc, open_valves, time)] = max(
        released_so_far, been_here.get((loc, open_valves, time), -1)
    )
    been_here[(loc, open_valves, time + 1)] = max(
        released_so_far, been_here.get((loc, open_valves, time), -1)
    )
    if otime < 26 and time >= 26:
        at_26 = released_so_far - ratelookup[open_valves] * (time - 26)
        best_to_26[open_valves] = max(best_to_26.get(open_valves, -1), at_26)
    if time == 30:
        best_so_far = max(best_so_far, released_so_far)
        continue
    nreleased = ratelookup[open_valves] + released_so_far
    for (dest, desttime) in enumerate(distmat[loc]):
        ntime = time + desttime
        if ntime <= 30:
            nreleased = ratelookup[open_valves] * desttime + released_so_far
            npot = nreleased + (30 - ntime) * ratelookup[open_valves]
            heapq.heappush(
                all_spots, (-npot, nreleased, dest, open_valves, ntime, time)
            )
    if loc != aa_int:
        valve = 1 << loc
        if not (valve & open_valves):
            ntime = time + 1
            nreleased = ratelookup[open_valves] + released_so_far
            nopen_valves = open_valves | valve
            npot = nreleased + (30 - ntime) * ratelookup[nopen_valves]
            heapq.heappush(
                all_spots, (-npot, nreleased, loc, nopen_valves, ntime, time)
            )
    # print("diag: ", len(all_spots), time)

# part 1
print(best_so_far)
# print(
#     f"Samples  nreleased + (30 - ntime) * ratelookup[open_valves]: {sample}, max_len: {max_len_all_spots}"
# )
# part 2
best_so_far = -1
for (op1, op2) in itertools.combinations(best_to_26, 2):
    if not (op1 & op2):
        best_so_far = max(best_so_far, best_to_26[op1] + best_to_26[op2])

print(best_so_far)
