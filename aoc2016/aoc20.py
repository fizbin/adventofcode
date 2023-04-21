import sys
import re

infile = "aoc20.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

ranges = []
for line in data:
    m = re.match(r"(\d+)-(\d+)", line)
    ranges.append((int(m.group(1)), 1 + int(m.group(2))))

candidate = 0
for low, high in sorted(ranges):
    if candidate > high:
        continue
    elif low <= candidate:
        candidate = high
    else:
        break
print(candidate)

open_list = [(0, 1 << 32)]
for low, high in sorted(ranges):
    new_open_list = []
    for oplow, ophigh in open_list:
        if ophigh <= low or high <= oplow:
            new_open_list.append((oplow, ophigh))
            continue
        if low <= oplow <= ophigh <= high:
            continue
        if oplow < low <= high < ophigh:
            new_open_list.extend([(oplow, low), (high, ophigh)])
            continue
        if oplow < low < ophigh <= high:
            new_open_list.append((oplow, low))
            continue
        if low <= oplow <= high < ophigh:
            new_open_list.append((high, ophigh))
            continue
        raise Exception(f"What to do with {(low,high)} removed from {(oplow,ophigh)}")
    open_list = new_open_list

print(sum(t[1] - t[0] for t in open_list))
