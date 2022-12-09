from aoc_util import get_data

data = get_data(6)
idx = 4
while len(set(data[idx - 4 : idx])) < 4:
    idx += 1
print(idx)

idx = 14
while len(set(data[idx - 14 : idx])) < 14:
    idx += 1
print(idx)
