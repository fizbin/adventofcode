import re
import sys
import heapq

with open("aoc19.in.txt" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read()

(rules_src, goal_molecule) = data.split('\n\n')
goal_molecule = goal_molecule.strip()
rules = re.findall(r'(\w+) => (\w+)', rules_src)
rules.sort(key=lambda t: len(t[1]))

visited = {}
workheap = [(-len(goal_molecule), 0, goal_molecule)]
i = 0
while workheap:
    (slen, steps, molecule) = heapq.heappop(workheap)
    i += 1
    if (i % 1) == 0:
        print("i", i, "queue", len(workheap), "working", slen, steps)
    if molecule == 'e':
        print("NSteps", steps)
        break
    for (from_str, to_str) in rules:
        for m in re.finditer(to_str, molecule):
            reduction = molecule[:m.start()] + from_str + molecule[m.end():]
            if visited.get(reduction, steps + 10) > steps + 1:
                visited[reduction] = steps + 1
                heapq.heappush(workheap, (len(reduction), steps + 1,
                                          reduction))
