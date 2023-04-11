import sys
import re

infile = "aoc2.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = infilep.read()

mysum = 0
for line in data.splitlines():
    numbers_str = re.findall(r"-?\d+", line)
    numbers = [int(x) for x in numbers_str]
    mysum += max(numbers) - min(numbers)
print(mysum)


class NextLine(Exception):
    pass


mysum = 0
for line in data.splitlines():
    numbers_str = re.findall(r"-?\d+", line)
    numbers = sorted(int(x) for x in numbers_str)
    try:
        for lidx in range(len(numbers)):
            for hidx in range(lidx + 1, len(numbers)):
                if numbers[hidx] % numbers[lidx] == 0:
                    mysum += numbers[hidx] // numbers[lidx]
                    raise NextLine()
        else:
            raise Exception(f"Can't find div pair in {line}")
    except NextLine:
        pass
print(mysum)
