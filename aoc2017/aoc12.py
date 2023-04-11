import sys
import re
import collections

infile = "aoc12.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

graph = {}
for line in data:
    m = re.match(r"(\d+) *<-> *([\d ,]*)", line)
    graph[int(m.group(1))] = [int(x) for x in re.findall(r"\d+", m.group(2))]

equiv = {x: x for x in graph}
changed = set(equiv)
while changed:
    ochanged = changed
    changed = set()
    for node in ochanged:
        for target in graph[node]:
            if equiv[node] != equiv[target]:
                if equiv[node] < equiv[target]:
                    changed.add(target)
                else:
                    changed.add(node)
                equiv[node] = min(equiv[node], equiv[target])
                equiv[target] = equiv[node]

graph_sizes = collections.Counter(equiv.values())
print(graph_sizes[0])
print(len(list(graph_sizes.keys())))
