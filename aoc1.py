#!/usr/bin/env python

from aoc_util import get_data

data0 = get_data(1).split("\n\n")
data = [[int(x) for x in d.splitlines()] for d in data0]

print(max(sum(foo) for foo in data))

print(sum(sorted(sum(foo) for foo in data)[-3:]))
