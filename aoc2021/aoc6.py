import sys
import re
import itertools

with open('aoc6.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = [int(x) for x in re.findall(r'\d+', f.read())]
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

def do_1_day(inval):
    new_8 = [8] * len([x for x in inval if x == 0])
    return [x-1 if x > 0 else 6 for x in inval] + new_8

datap = data
for d in range(80):
    #print(datap)
    datap = do_1_day(datap)

print(len(datap))
del(datap)

counts = {}
for x in data:
    counts[x] = counts.get(x, 0) + 1

def do_one(inval):
    outval = {x:0 for x in range(0,9)}
    for x in range(0,9):
        if x in inval:
            if x == 0:
                outval[8] += inval[x]
                outval[6] += inval[x]
            else:
                outval[x-1] += inval[x]
    return outval

countp = counts
for _ in range(256):
    countp = do_one(countp)

print(sum(countp.values()))
