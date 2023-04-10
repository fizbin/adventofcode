#!/usr/bin/env python

from aoc_util import get_data_lines, numbers

data = get_data_lines(4)
badlines = 0
for line in data:
    (elfa1, elfa2, elfb1, elfb2) = numbers(line)
    if (elfa1 <= elfb1 and elfb2 <= elfa2) or (elfb1 <= elfa1 and elfa2 <= elfb2):
        badlines += 1

print(badlines)

badlines = 0
for line in data:
    (elfa1, elfa2, elfb1, elfb2) = numbers(line)
    if not ((elfa2 < elfb1) or (elfa1 > elfb2)):
        badlines += 1

print(badlines)
