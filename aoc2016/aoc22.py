import sys
import itertools
import re

infile = "aoc22.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

nodes = []

for line in data:
    if m := re.match(r"/dev/grid/node-x(\d+)-y(\d+) *(\d+)T *(\d+)T *(\d+)T", line):
        nodes.append(
            (int(m.group(1)), int(m.group(2)), int(m.group(4)), int(m.group(5)))
        )

retval = 0
for nodeA, nodeB in itertools.permutations(nodes, 2):
    if 0 < nodeA[2] <= nodeB[3]:
        retval += 1
        if nodeB[0:2] != (11, 22):
            print(nodeA[0:2], "->", nodeB[0:2])
print(retval)

for node in nodes:
    if node[1] == 0:
        print()
    if node[2] == 0:
        print("_", end="")
    elif node[2] > 100:
        print("#", end="")
    else:
        print(".", end="")
print()
