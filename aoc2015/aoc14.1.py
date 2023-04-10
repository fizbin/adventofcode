import re
import itertools
import sys

with open("aoc14.in.txt" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

secs = 2503
max_travel = 0

for ln in data:
    m = re.match(r'\D*(-?\d+) km/s for (\d+).*but then must\D*(\d+)', ln)
    (speed, dur, rest) = map(int, m.groups())
    print([speed] * dur + [0] * rest)
    travel = sum(itertools.islice(
        itertools.cycle([speed] * dur + [0] * rest), secs))
    max_travel = max(max_travel, travel)

print(max_travel)
