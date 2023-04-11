import sys
import re

infile = "aoc4.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = infilep.read()

mysum = 0
for line in data.splitlines():
    words = re.findall(r"\w+", line)
    if len(words) == len(set(words)):
        mysum += 1
print(mysum)

mysum = 0
for line in data.splitlines():
    words = re.findall(r"\w+", line)
    if len(words) == len(set("".join(sorted(x)) for x in words)):
        mysum += 1
print(mysum)
