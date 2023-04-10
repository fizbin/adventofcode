"""
Advent of code day 12 in python.
"""

import sys
import re
import collections
import itertools
import copy
import functools

with open("aoc12.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
    # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    data = re.findall(r"(\w+)-(\w+)", f.read())
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

nodes = set()
graph = {}
for (left, right) in data:
    graph.setdefault(left, []).append(right)
    graph.setdefault(right, []).append(left)
    nodes.add(left)
    nodes.add(right)

caps = set(x for x in nodes if re.match("^[A-Z]+$", x))


@functools.lru_cache
def countways(visited, where):
    if where == "end":
        return 1
    candidates = set(graph[where]) - set(visited)
    if not candidates:
        return 0
    retval = 0
    if where not in caps:
        nvisited = visited | frozenset([where])
    else:
        nvisited = visited
    for c in candidates:
        retval += countways(nvisited, c)
    return retval


@functools.lru_cache
def countways2(visited, small, where):
    if where == "end":
        # print(sofar)
        return 1
    nsmall = small
    if nsmall is None and where not in visited:
        candidates = set(graph[where]) - set(["start"])
    else:
        candidates = set(graph[where]) - set(visited)
    if not candidates:
        return 0
    retval = 0
    if where not in caps:
        if where in visited:
            nsmall = where
        nvisited = visited | frozenset([where])
    else:
        nvisited = visited
    for c in candidates:
        retval += countways2(nvisited, nsmall, c)
    return retval


print(countways(frozenset(), "start"))
print(countways2(frozenset(), None, "start"))
