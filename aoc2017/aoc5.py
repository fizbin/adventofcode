import sys
import re

infile = "aoc5.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = [int(x) for x in re.findall(r"-?\d+", infilep.read())]

og_data = list(data)

ip = 0
steps = 0
while 0 <= ip < len(data):
    oip = ip
    ip += data[oip]
    data[oip] += 1
    steps += 1
print(steps)

data = list(og_data)
ip = 0
steps = 0
while 0 <= ip < len(data):
    oip = ip
    ip += data[oip]
    if data[oip] >= 3:
        data[oip] -= 1
    else:
        data[oip] += 1
    steps += 1
print(steps)
