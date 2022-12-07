from aoc_util import get_data_lines
import re

data = get_data_lines(7)
dir_total_sizes = {"": 0}
cwd = []
for line in data[1:]:
    if line.startswith("$ cd"):
        if line == "$ cd ..":
            cwd.pop()
        else:
            cwd.append(line.split()[2])
    elif line == "$ ls":
        continue
    elif line.startswith("dir "):
        newdirname = "/".join(cwd + [line.split()[1]])
        dir_total_sizes[newdirname] = 0
    else:
        m = re.match("(\d+) (\S+)$", line)
        (sizestr, filename) = (m.group(1), m.group(2))
        size = int(sizestr)
        for idx in range(len(cwd) + 1):
            cwdstr = "/".join(cwd[0:idx])
            dir_total_sizes[cwdstr] += size

tot = 0
for (k, v) in dir_total_sizes.items():
    if v <= 100000:
        tot += v
print(tot)

remaining = 70000000 - dir_total_sizes[""]
needed = 30000000 - remaining
print(min(x for x in dir_total_sizes.values() if x >= needed))
