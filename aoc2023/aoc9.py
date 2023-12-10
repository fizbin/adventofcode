#!/usr/bin/env python

from aoc_util import get_data_lines, numbers


def infer_next(dots):
    if all(x == 0 for x in dots):
        return 0
    diff_line = [x - y for (x, y) in zip(dots[1:], dots)]
    next_diff = infer_next(diff_line)
    return dots[-1] + next_diff


data = get_data_lines(9)

newtot = 0
for ln in data:
    vals = numbers(ln)
    nval = infer_next(vals)
    newtot += nval
print(newtot)

newtot = 0
for ln in data:
    vals = list(reversed(numbers(ln)))
    nval = infer_next(vals)
    newtot += nval
print(newtot)
