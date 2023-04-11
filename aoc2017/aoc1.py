import sys

infile = "aoc1.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = infilep.read()

data = data.strip(" \r\n\t")
# print(repr(data))

# part 1
mysum = 0
for x, y in zip(data[-1] + data[:-1], data):
    if x == y:
        mysum += int(x)
print(mysum)

# part 2
mysum = 0
for x, y in zip(data[len(data) // 2 :] + data[: len(data) // 2], data):
    if x == y:
        mysum += int(x)
print(mysum)
