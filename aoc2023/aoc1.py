#!/usr/bin/env python

from aoc_util import get_data_lines
import re

data = get_data_lines(1)
numstrs = ["".join(re.findall("[0-9]", line)) for line in data]
nums = [int(x[0] + x[-1]) for x in numstrs]

print(sum(nums))

dignames = "one,two,three,four,five,six,seven,eight,nine".split(",")
dignamesre = "|".join(dignames)
digvalues = dict(zip(dignames, range(1, 10)))
for d in range(10):
    digvalues[str(d)] = int(d)

firstnums = [re.findall(r"(" + dignamesre + "|[0-9])", x)[0] for x in data]
lastnums = [re.findall(r".*(" + dignamesre + "|[0-9])", x)[0] for x in data]
nums = [int(str(digvalues[a]) + str(digvalues[b])) for (a, b) in zip(firstnums, lastnums)]

print(sum(nums))
