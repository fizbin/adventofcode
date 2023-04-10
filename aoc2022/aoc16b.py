from aoc_util import get_data_lines
import re
import itertools
from typing import Dict, Tuple

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

# state is (released_so_far, loc, open_valves, time)

def do_minute(currstate: Dict[Tuple[str, frozenset[str]], int]):
    newstate: Dict[Tuple[str, frozenset[str]], int] = {}
    for ((loc, open_valves), released_so_far) in currstate.items():
        nreleased = released_so_far + sum(valverates[v] for v in open_valves)
        # three possible actions: stay, move, or open valve
        # stay
        newstate[(loc, open_valves)] = max(newstate.get((loc, open_valves), -1), nreleased)
        for dest in valvemap[loc]:
            # move
            newstate[(dest, open_valves)] = max(newstate.get((dest, open_valves), -1), nreleased)
        if loc not in open_valves and valverates[loc] > 0:
            # open valve
            nopen_valves = frozenset(open_valves | frozenset([loc]))
            newstate[(loc, nopen_valves)] = max(newstate.get((loc, nopen_valves), -1), nreleased)
    currstate.update(newstate)

mystate = {("AA", frozenset()): 0}
for tick in range(30):
    do_minute(mystate)

print(max(mystate.values()))

mystate = {("AA", frozenset()): 0}
for tick in range(26):
    do_minute(mystate)

by_open_valves = {}
for ((loc, open_valves), n) in mystate.items():
    by_open_valves[open_valves] = max(by_open_valves.get(open_valves, -1), n)

best_so_far = -1
for (open1, open2) in itertools.combinations(by_open_valves, 2):
    if not (open1 & open2):
        best_so_far = max(best_so_far, by_open_valves[open1] + by_open_valves[open2])

print(best_so_far)
