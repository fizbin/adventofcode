import re
import itertools
import sys

with open("aoc14.in.txt" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

secs = 2503
max_travel = 0

iters = []
for ln in data:
    m = re.match(r'\D*(-?\d+) km/s for (\d+).*but then must\D*(\d+)', ln)
    (speed, dur, rest) = map(int, m.groups())
    iters.append(itertools.cycle([speed] * dur + [0] * rest))

scores = [0] * len(iters)
dists = [0] * len(iters)
for sec in range(secs):
    maxdist = 0
    for (n, it) in enumerate(iters):
        dists[n] += next(it)
        maxdist = max(maxdist, dists[n])
    for n in range(len(iters)):
        if dists[n] == maxdist:
            scores[n] += 1

print(max(scores))
