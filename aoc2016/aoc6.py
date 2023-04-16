import sys
import collections


infile = "aoc6.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

counters: list[collections.Counter] = []

with open(infile, "r", encoding="utf-8") as infilep:
    data = [x.strip() for x in infilep]

for _ in range(len(data[0])):
    counters.append(collections.Counter())

for line in data:
    for idx, char in enumerate(line):
        counters[idx].update(char)

print("".join(c.most_common(1)[0][0] for c in counters))

print("".join(c.most_common()[-1][0] for c in counters))
