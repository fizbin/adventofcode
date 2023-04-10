#!/usr/bin/env python3
import sys
import functools

with open('aoc10.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = [int(x) for x in list(f)]

adapters = sorted(data)
adapters += [max(data)+3]

diffs1 = 0
diffs3 = 0

current = 0
for adapt in adapters:
    if current - adapt == -1:
        diffs1 += 1
    elif current - adapt == -2:
        pass
    elif current - adapt == -3:
        diffs3 += 1
    else:
        raise Exception(str((current, adapt)))
    current = adapt

print(diffs1, diffs3, diffs1*diffs3)

maxadapt = max(adapters)

@functools.lru_cache(maxsize=None)
def combofrom(joltage):
    if joltage == maxadapt:
        return 1
    total = 0
    for diff in (1, 2, 3):
        if joltage + diff in adapters:
            total += combofrom(joltage + diff)
    return total

print(combofrom(0))
