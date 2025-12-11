import sys
from typing import Sequence
import aoc_util
import functools
import itertools
import re


def distsq(a: Sequence[int], b: Sequence[int]) -> int:
    return (a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2


def part1(data, n_to_connect) -> int:
    all_idx_pairs = list(itertools.combinations(range(len(data)), 2))
    all_idx_pairs.sort(key=lambda p: distsq(data[p[0]], data[p[1]]))
    connected: dict[int, list[int]] = {}
    # print("DBG>", all_idx_pairs[0:n_to_connect])
    for pair in all_idx_pairs[0:n_to_connect]:
        connected.setdefault(pair[0], []).append(pair[1])
        connected.setdefault(pair[1], []).append(pair[0])
    group_nums = {x: x for x in range(len(data))}
    changed = list(range(len(data)))
    while changed:
        nchanged = set()
        for y in changed:
            nbs = connected.get(y, [])
            for x in nbs:
                if group_nums[x] > group_nums[y]:
                    group_nums[x] = group_nums[y]
                    nchanged.add(x)
        changed = sorted(nchanged)
    groups: dict[int, list[int]] = {}
    for x in group_nums:
        groups.setdefault(group_nums[x], []).append(x)
    sizes = [len(g) for g in groups.values()]
    sizes.sort(reverse=True)

    return sizes[0] * sizes[1] * sizes[2]


def part2(data: list[list[int]], n_to_connect) -> int:
    all_idx_pairs = list(itertools.combinations(range(len(data)), 2))
    all_idx_pairs.sort(key=lambda p: distsq(data[p[0]], data[p[1]]))
    connected: dict[int, list[int]] = {}
    for pair in all_idx_pairs[0:n_to_connect]:
        connected.setdefault(pair[0], []).append(pair[1])
        connected.setdefault(pair[1], []).append(pair[0])
    group_nums = {x: x for x in range(len(data))}
    changed = list(range(len(data)))
    while changed:
        nchanged = set()
        for y in changed:
            nbs = connected.get(y, [])
            for x in nbs:
                if group_nums[x] > group_nums[y]:
                    group_nums[x] = group_nums[y]
                    nchanged.add(x)
        changed = sorted(nchanged)
    pair_idx: int = n_to_connect - 1
    while max(group_nums.values()) > 0:
        pair_idx += 1
        pair_to_connect: tuple[int, int] = all_idx_pairs[pair_idx]
        connected.setdefault(pair_to_connect[0], []).append(pair_to_connect[1])
        connected.setdefault(pair_to_connect[1], []).append(pair_to_connect[0])

        changed = list(range(len(data)))
        while changed:
            nchanged = set()
            for y in changed:
                nbs = connected.get(y, [])
                for x in nbs:
                    if group_nums[x] > group_nums[y]:
                        group_nums[x] = group_nums[y]
                        nchanged.add(x)
            changed = sorted(nchanged)
    return data[pair_to_connect[0]][0] * data[pair_to_connect[1]][0]


if __name__ == "__main__":
    datalines = aoc_util.get_data_lines(8)
    n_to_connect = 1000
    if len(sys.argv) >= 2:
        n_to_connect = int(sys.argv[1])
    data = [[int(v) for v in l.split(",")] for l in datalines]
    print(part1(data, n_to_connect))
    print(part2(data, n_to_connect))
