"""
Advent of code day 14 in python.
"""

import sys
import re
import collections

with open("aoc14.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
    # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    # data = re.findall(r"(\w+)-(\w+)", f.read())
    data = f.read()
    thing1 = re.findall(r"[A-Z]{3,}", data)[0]
    thing2 = re.findall(r"(\w\w) -> (\w)", data)
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

rules = {a[0]: a[1] for a in thing2}


def dostep1(poly):
    "Do substitution step with polymer as string"
    retval = ""
    for (fst, snd) in zip(poly, poly[1:]):
        if fst + snd in rules:
            retval += fst + rules[fst + snd]
        else:
            retval += fst
    retval += poly[-1]
    return retval


mypoly1 = thing1
for _ in range(10):
    mypoly1 = dostep1(mypoly1)

cou = collections.Counter(mypoly1)
((_, mostcc),) = cou.most_common(1)
(_, leastcc) = cou.most_common()[-1]

print(mostcc - leastcc)


def dostep2(poly: collections.Counter):
    "Do substitution step with polymer as Counter"
    retval = collections.Counter()
    for ((fst, snd), cnt) in poly.items():
        if fst + snd in rules:
            mid = rules[fst + snd]
            retval.update({(fst + mid): cnt, (mid + snd): cnt})
        else:
            retval.update({(fst + snd): cnt})
    return retval


mypoly2 = collections.Counter([a + b for (a, b) in zip(thing1, thing1[1:])])
for _ in range(40):
    mypoly2 = dostep2(mypoly2)

cou = collections.Counter()
for (pair, pcnt) in mypoly2.items():
    cou.update({pair[0]: pcnt})
cou.update({thing1[-1]: 1})
((_, mostcc),) = cou.most_common(1)
(_, leastcc) = cou.most_common()[-1]

print(mostcc - leastcc)
