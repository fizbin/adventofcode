from typing import Sequence
from aoc_util import get_data_lines

data = get_data_lines(11)


def advance(stone):
    if stone == 0:
        return (1,)
    stonestr = str(stone)
    if len(stonestr) % 2 == 0:
        return (int(stonestr[: len(stonestr) // 2]), int(stonestr[len(stonestr) // 2 :]))
    return (stone * 2024,)


total = 0
for stonestr in data[0].split():
    stones: Sequence[int] = (int(stonestr),)
    for _ in range(25):
        nstones = []
        for stone in stones:
            nstones.extend(advance(stone))
        stones = nstones
    total += len(stones)

print("Part 1:", total)

# part 2

total = 0
for stonestr in data[0].split():
    freq = {int(stonestr): 1}
    for _ in range(75):
        nstones = []
        for stone, dup in freq.items():
            nstones.extend((x, dup) for x in advance(stone))
        freq = {}
        for stone, dup in nstones:
            freq[stone] = freq.get(stone, 0) + dup
    for dup in freq.values():
        total += dup

print("Part 2:", total)
