import sys
import re
import itertools

with open('aoc7.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = [int(x) for x in re.findall(r'\d+', f.read())]
    # data = re.findall(r'\S+', f.read())
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

def cost_of(target):
    return sum(abs(x-target) for x in data)

best_target_cost = 99999999999999
for target in range(min(data), max(data)+1):
    best_target_cost = min([best_target_cost, cost_of(target)])

print(best_target_cost)

def cost_of2(target):
    retval = 0
    for x in data:
        dist = abs(x-target)
        retval += dist*(dist+1)//2
    return retval

best_target_cost = 99999999999999
for target in range(min(data), max(data)+1):
    best_target_cost = min([best_target_cost, cost_of2(target)])

print(best_target_cost)