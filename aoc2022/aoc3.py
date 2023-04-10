#!/usr/bin/env python

from aoc_util import get_data_lines

data = get_data_lines(3)
priority = {}
for ch in range(ord("a"), ord("z") + 1):
    priority[chr(ch)] = ch - ord("a") + 1
for ch in range(ord("A"), ord("Z") + 1):
    priority[chr(ch)] = ch - ord("A") + 27

tot = 0
for sack in data:
    first = set(sack[0 : len(sack) // 2])
    second = set(sack[len(sack) // 2 :])
    inter = first & second
    assert len(inter) == 1
    tot += priority[min(inter)]
print(tot)

tot = 0
for grpidx in range(0, len(data), 3):
    first = set(data[grpidx])
    second = set(data[grpidx + 1])
    third = set(data[grpidx + 2])
    inter = first & second & third
    assert len(inter) == 1
    tot += priority[min(inter)]
print(tot)
