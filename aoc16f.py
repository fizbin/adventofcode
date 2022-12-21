from aoc_util import get_data_lines
import re
import heapq
from typing import Dict, Tuple
import itertools
import functools
import numpy as np

data = get_data_lines(16)

# Just make the naive recursive call and use @functools.cache

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

# part 1


@functools.cache
def get_pressure_released(loc, time_left, valves):
    if time_left == 0:
        return 0
    this_released = sum(valverates[v] for v in valves)
    best_so_far = this_released + get_pressure_released(loc, time_left - 1, valves)
    if valverates.get(loc) and loc not in valves:
        best_so_far = max(
            best_so_far,
            this_released
            + get_pressure_released(loc, time_left - 1, frozenset([loc]) | valves),
        )
    for dest in valvemap[loc]:
        best_so_far = max(
            best_so_far,
            this_released + get_pressure_released(dest, time_left - 1, valves),
        )
    return best_so_far


print(get_pressure_released("AA", 30, frozenset()))

# part 2


@functools.cache
def get_pressure_released_map(loc, time_left, valves):
    if time_left == 0:
        return {valves: 0}
    this_released = sum(valverates[v] for v in valves)
    best_so_far = dict(
        (k, v + this_released)
        for (k, v) in get_pressure_released_map(loc, time_left - 1, valves).items()
    )
    if valverates.get(loc) and loc not in valves:
        for (k, v) in get_pressure_released_map(
            loc, time_left - 1, frozenset([loc]) | valves
        ).items():
            best_so_far[k] = max(this_released + v, best_so_far.get(k, 0))
    for dest in valvemap[loc]:
        for (k, v) in get_pressure_released_map(dest, time_left - 1, valves).items():
            best_so_far[k] = max(this_released + v, best_so_far.get(k, 0))
    return best_so_far


all_solutions = get_pressure_released_map("AA", 26, frozenset())

best_so_far = 0
for (k1, k2) in itertools.combinations(all_solutions, 2):
    if not (k1 & k2):
        best_so_far = max(best_so_far, all_solutions[k1] + all_solutions[k2])

print(best_so_far)
