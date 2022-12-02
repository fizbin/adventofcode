#!/usr/bin/env python

from aoc_util import *
import re

data = get_data_lines(2)
wins = set(('A Y', 'B Z', 'C X'))
ties = set(('A X', 'B Y', 'C Z'))
score = 0
for game in data:
    if game in wins:
        score += 6
    elif game in ties:
        score += 3
    if game.endswith('X'):
        score += 1
    if game.endswith('Y'):
        score += 2
    if game.endswith('Z'):
        score += 3
print(score)

bepaper = set(('B Y', 'A Z', 'C X'))
bescissors = set(('C Y', 'B Z', 'A X'))
score = 0
for game in data:
    if game in bescissors:
        score += 3
    elif game in bepaper:
        score += 2
    else:
        score += 1
    if game.endswith('Y'):
        score += 3
    if game.endswith('Z'):
        score += 6

print(score)