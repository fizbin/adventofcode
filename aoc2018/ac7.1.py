import sys
import re

deps = []
with open('ac7.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    for line in f:
        m = re.match('Step ([A-Z]) must be finished before step ([A-Z]) can',
                     line)
        deps.append(m.group(1, 2))

all_steps = set(x for (x, _) in deps) | set(y for (_, y) in deps)
done_steps = []
done_set = set(done_steps)

while len(done_steps) < len(all_steps):
    next_steps = all_steps - done_set
    undone_deps = set(y for (x, y) in deps if x not in done_set)
    next_step = min(next_steps - undone_deps)
    done_steps.append(next_step)
    done_set.add(next_step)

print(''.join(done_steps))
