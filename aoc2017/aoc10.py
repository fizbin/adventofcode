import sys
import re
import operator
import functools

infile = "aoc10.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    raw_data = infilep.read().strip()
    data = [int(x) for x in re.findall(r"-?\d+", raw_data)]

mylist = list(range(256))
skip = 0
cp = 0
for length in data:
    listy = mylist[cp:] + mylist[:cp]
    listy[0:length] = list(reversed(listy[0:length]))
    mylist = listy[len(listy) - cp :] + listy[: len(listy) - cp]
    cp += length + skip
    cp %= len(mylist)
    skip += 1
print(mylist[0] * mylist[1])

# part 2
data = [ord(x) for x in raw_data] + [17, 31, 73, 47, 23]
mylist = list(range(256))
skip = 0
cp = 0
for _ in range(64):
    for length in data:
        listy = mylist[cp:] + mylist[:cp]
        listy[0:length] = list(reversed(listy[0:length]))
        mylist = listy[len(listy) - cp :] + listy[: len(listy) - cp]
        cp += length + skip
        cp %= len(mylist)
        skip += 1

hash = ""
for start in range(0, 256, 16):
    val = functools.reduce(operator.xor, mylist[start : start + 16], 0)
    hash += "%02x" % (val,)
print(hash)
