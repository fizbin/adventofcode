import sys

with open('aoc1.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = [int(x) for x in f]

count = 0
prev = 999999999999
for d in data:
    if prev < d:
        count += 1
    prev = d
print(count)

data2 = [data[i] + data[i+1] + data[i+2] for i in range(len(data)-2)]
count = 0
prev = 999999999999
for d in data2:
    if prev < d:
        count += 1
    prev = d
print(count)
