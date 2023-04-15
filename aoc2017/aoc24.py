import sys
import collections

infile = "aoc24.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = [x.strip() for x in list(infilep)]

bars = [(int(x), int(y)) for line in data for (x, y) in [line.split("/")]]

dcount = collections.Counter()
match_bars = {}
for bar in bars:
    dcount.update(bar)
    match_bars.setdefault(bar[0], []).append(bar)
    match_bars.setdefault(bar[1], []).append(bar)

workqueue = [(0, 0, frozenset())]
max_strength = 0
max_strength_longest = (0, 0)
while workqueue:
    (where, how_far, path) = workqueue.pop()
    max_strength = max(how_far, max_strength)
    max_strength_longest = max((len(path), how_far), max_strength_longest)
    for bar in match_bars[where]:
        if bar not in path:
            oend = sum(bar) - where
            workqueue.append((oend, how_far + sum(bar), path | frozenset((bar,))))

print(max_strength)
print(max_strength_longest[1])
