import re
from aoc_util import get_data_paras

data = get_data_paras(25)

locks = []
keys = []
for para in data:
    lines = para.splitlines()
    if re.match(r"^#+$", lines[0]):
        heights = [0] * len(lines[0])
        for line in lines[1:]:
            for hidx, ch in enumerate(line):
                if ch == "#":
                    heights[hidx] += 1
        locks.append(tuple(heights))
    elif re.match(r"^#+$", lines[-1]):
        # key
        heights = [0] * len(lines[0])
        for line in lines[:-1]:
            for hidx, ch in enumerate(line):
                if ch == "#":
                    heights[hidx] += 1
        keys.append(tuple(heights))
    else:
        print(f"ERROR! Bad para:\n\n{para}\n")
        exit(1)

total = 0
for lock in locks:
    for key in keys:
        if all(x + y < 6 for (x, y) in zip(lock, key)):
            total += 1
print("Part 1:", total)
