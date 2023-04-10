import re
import sys

with open('aoc2.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

total = 0
total_ribbon = 0
for ln in data:
    (x, y, z) = sorted(map(int, re.findall(r'\d+', ln)))
    total += 2*(x*y + y*z + x*z) + x*y
    total_ribbon += 2*(x+y) + x*y*z

print(total)
print(total_ribbon)
