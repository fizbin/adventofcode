from aoc_util import get_data
import re

data = get_data(3)
total = 0
for m in re.finditer(r"mul\((\d+), *(\d+)\)", data):
    total += int(m.group(1)) * int(m.group(2))
print("Part 1:", total)


total = 0
enabled = True
for m in re.finditer(r"(do\(\))|(don't\(\))|mul\((\d+), *(\d+)\)", data):
    if m.group(1):
        enabled = True
    elif m.group(2):
        enabled = False
    else:
        if enabled:
            total += int(m.group(3)) * int(m.group(4))
print("Part 2:", total)
