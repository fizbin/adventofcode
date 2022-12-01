#!/usr/bin/env python

from aoc_util import get_data

data = get_data(1)
data = data.split("\n\n")
data = [[int(x) for x in d.splitlines()] for d in data]

print(max(sum(foo) for foo in data))

print(sum(sorted(sum(foo) for foo in data)[-3:]))
