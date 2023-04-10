import numpy as np
import sys
import re

with open('aoc16.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

initial = [int(x) for x in re.findall(r'\d', data[0])] * 10000
work = np.array(initial).astype(int)
offset = int(''.join(str(x) for x in initial[0:7]))
print(offset, len(initial))

work = work[offset:]

for n in range(100):
    print("Start", n)
    sum = 0
    for idx in reversed(range(len(work))):
        sum += work[idx]
        sum %= 10
        work[idx] = sum
    print("Done", n)

print(work[0:8])
