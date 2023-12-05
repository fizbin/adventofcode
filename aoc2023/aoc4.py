#!/usr/bin/env python

from aoc_util import get_data_lines, numbers
import re

data = get_data_lines(4)
total = 0
card_total = {}
for line in data:
    m = re.match("Card *(\d+): (.*)[|](.*)", line)
    card_total[int(m.group(1))] = 1
for line in data:
    m = re.match("Card *(\d+): (.*)[|](.*)", line)
    winning = set(numbers(m.group(2)))
    have = set(numbers(m.group(3)))
    nisect = len(winning & have)
    if nisect:
        total += 1 << (nisect - 1)
        cardn = int(m.group(1))
        for idx in range(cardn + 1, cardn + 1 + nisect):
            card_total[idx] += card_total[cardn]
print(total)
print(sum(card_total.values()))
