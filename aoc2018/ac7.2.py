import re

deps = []
with open('ac7.in.txt') as f:
    for line in f:
        m = re.match('Step ([A-Z]) must be finished before step ([A-Z]) can',
                     line)
        deps.append(m.group(1, 2))

all_steps = set(x for (x, _) in deps) | set(y for (_, y) in deps)
done_steps = []
done_set = set(done_steps)
time_actions = {}
working = ['.', '.', '.', '.', '.']
now = 0

while len(done_steps) < len(all_steps):
    undone_deps = set(y for (x, y) in deps if x not in done_set)
    next_steps = all_steps - done_set - undone_deps - set(working)
    for (worker, work) in list(enumerate(working)):
        if next_steps and work == '.':
            working[worker] = min(next_steps)
            next_steps.remove(working[worker])
            duration = 61 + (ord(working[worker]) - ord('A'))
            time_actions.setdefault(now+duration, []).append(worker)
    now = now + 1
    for worker in time_actions.get(now, []):
        f = working[worker]
        working[worker] = '.'
        done_set.add(f)
        done_steps.append(f)

print(now)
print(''.join(done_steps))
