import sys
import re
import collections

with open('aoc9.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    #data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    data = re.findall(r'\S+', f.read())
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

for (idx, line) in enumerate(data):
    data[idx] = list(line)

for line in data:
    for (idx, x) in enumerate(line):
        line[idx] = int(x)

total = 0
for row in range(len(data)):
    for col in range(len(data[row])):
        if row > 0 and data[row-1][col] <= data[row][col]:
            continue
        if row < len(data) - 1 and data[row+1][col] <= data[row][col]:
            continue
        if col > 0 and data[row][col-1] <= data[row][col]:
            continue
        if col < len(data[row])-1 and data[row][col+1] <= data[row][col]:
            continue
        total += data[row][col] + 1

print(total)

basin = [[0] * len(data[0]) for _ in range(len(data))]
basin_n = 1
for row in range(len(data)):
    for col in range(len(data[row])):
        if data[row][col] != 9:
            basin[row][col] = basin_n
            basin_n += 1

keep_going = True
while keep_going:
    keep_going = False
    for row in range(len(data)):
        for col in range(len(data[row])):
            if basin[row][col] == 0:
                continue
            new_basin = basin[row][col]
            if row > 0 and basin[row-1][col] != 0:
                new_basin = max([new_basin, basin[row-1][col]])
            if row < len(data) - 1 and basin[row+1][col] != 0:
                new_basin = max([new_basin, basin[row+1][col]])
            if col > 0 and basin[row][col-1] != 0:
                new_basin = max([new_basin, basin[row][col-1]])
            if col < len(data[row])-1:
                new_basin = max([new_basin, basin[row][col+1]])
            if new_basin != basin[row][col]:
                basin[row][col] = new_basin
                keep_going = True

basinmap = collections.Counter()
for mybasin in sum(basin, []):
    basinmap[mybasin] += 1
del basinmap[0]

common = [x[1] for x in basinmap.most_common(3)]
print(common[0]*common[1]*common[2])
