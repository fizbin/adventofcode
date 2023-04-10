"""
Advent of code day 10 in python.
"""

import sys
import re

# import collections

with open("aoc10.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
    # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    data = re.findall(r"\S+", f.read())
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

score = {")": 3, "]": 57, "}": 1197, ">": 25137}
match = {"<": ">", "{": "}", "[": "]", "(": ")"}


def score_line(line):
    "Score a line for part 1"
    stack = []
    for sym in line:
        if sym in match:
            stack.append(match[sym])
        elif stack and sym == stack[-1]:
            stack.pop()
        else:
            return score[sym]
    return 0


score2 = {")": 1, "]": 2, "}": 3, ">": 4}


def score_line2(line):
    "Score a line for part 2"
    stack = []
    for sym in line:
        if sym in match:
            stack.append(match[sym])
        elif stack and sym == stack[-1]:
            stack.pop()
        else:
            return 0
    tot = 0
    for sym in reversed(stack):
        tot *= 5
        tot += score2[sym]
    return tot


print(sum(score_line(l) for l in data))
scores = sorted(score_line2(l) for l in data if score_line2(l) > 0)
print(scores[len(scores) // 2])
