"""
Advent of code day 15 in python.
"""

import sys
import re
import heapq

with open("aoc15.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
    # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    # data = re.findall(r"(\w+)-(\w+)", f.read())
    data = re.findall(r"\d+", f.read())
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    data = [list(int(y) for y in line) for line in data]


def doit(mydata):
    working = [(0, (0, 0))]
    heapq.heapify(working)
    seen = set()
    goal = (len(mydata) - 1, len(mydata[0]) - 1)
    while working:
        (cost, (row, col)) = heapq.heappop(working)
        if (row, col) == goal:
            return cost
        if (row, col) in seen:
            continue
        seen.add((row, col))
        if row > 1:
            heapq.heappush(working, (cost + mydata[row - 1][col], (row - 1, col)))
        if col > 1:
            heapq.heappush(working, (cost + mydata[row][col - 1], (row, col - 1)))
        if row < len(mydata) - 1:
            heapq.heappush(working, (cost + mydata[row + 1][col], (row + 1, col)))
        if col < len(mydata[0]) - 1:
            heapq.heappush(working, (cost + mydata[row][col + 1], (row, col + 1)))


print(doit(data))


def inc_row(datarow):
    return [(i % 9) + 1 for i in datarow]


bigdatahoriz = [
    sum(
        [
            dataline,
            inc_row(dataline),
            inc_row(inc_row(dataline)),
            inc_row(inc_row(inc_row(dataline))),
            inc_row(inc_row(inc_row(inc_row(dataline)))),
        ],
        [],
    )
    for dataline in data
]
bigdata = sum(
    [
        bigdatahoriz,
        [inc_row(r) for r in bigdatahoriz],
        [inc_row(inc_row(r)) for r in bigdatahoriz],
        [inc_row(inc_row(inc_row(r))) for r in bigdatahoriz],
        [inc_row(inc_row(inc_row(inc_row(r)))) for r in bigdatahoriz],
    ],
    [],
)

print(doit(bigdata))
