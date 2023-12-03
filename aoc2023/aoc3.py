#!/usr/bin/env python

from aoc_util import get_data_lines
import re

data = get_data_lines(3)

fulldata = ["." * len(data[0])] + data + ["." * len(data[0])]
fulldata = ["." + x + "." for x in fulldata]
gear_map = {}
total = 0
for row in range(1, len(fulldata)):
    for num_match in re.finditer(r"\d+", fulldata[row]):
        s, e = num_match.start(0), num_match.end(0)
        num = int(num_match.group(0))
        num2 = num
        surrounding_spots = (
            [(row, s - 1), (row, e)]
            + [(row - 1, c) for c in range(s - 1, e + 1)]
            + [(row + 1, c) for c in range(s - 1, e + 1)]
        )
        for i, j in surrounding_spots:
            ch = fulldata[i][j : j + 1]
            if re.match(r"[^\d.\s]", ch):
                total += num
                num = 0
            if ch == "*":
                gear_map.setdefault((i, j), []).append(num2)
print(total)

total_gear = 0
for adjs in gear_map.values():
    if len(adjs) == 2:
        total_gear += adjs[0] * adjs[1]
print(total_gear)
