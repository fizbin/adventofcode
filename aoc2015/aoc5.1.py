import sys
import re

with open('aoc5.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

re1 = re.compile(r'(?:[^aeiou]*[aeiou]){3}')
re2 = re.compile(r'(\w)\1')
re3 = re.compile(r'ab|cd|pq|xy')

nice = 0
for ln in data:
    if re1.match(ln) and re2.search(ln) and not re3.search(ln):
        nice += 1

print(nice)
