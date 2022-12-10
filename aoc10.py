from aoc_util import get_data_lines

data = get_data_lines(10)
xvals = [1]
for line in data:
    if line == "noop":
        xvals.append(xvals[-1])
    else:
        (a, b) = line.split()
        xvals.append(xvals[-1])
        xvals.append(xvals[-1] + int(b))

tot = 0
for idx in (20, 60, 100, 140, 180, 220):
    tot += idx * xvals[idx - 1]

print(tot)
print()

idx = 0
for row in range(6):
    for col in range(40):
        if abs(xvals[idx] - col) <= 1:
            print("\u2588", end="")
        else:
            print(".", end="")
        idx += 1
    print()
