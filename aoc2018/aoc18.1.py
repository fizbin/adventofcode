import re
import copy
import numpy as np
import sys

data = open('aoc18.in.txt').read()

ground = [list(ln.strip()) for ln in data.splitlines()]

def get(y, x):
    if y < 0 or x < 0:
        return ' '
    try:
        return ground[y][x]
    except IndexError:
        return ' '

#print('\n'.join(''.join(row) for row in ground))
#print('')

for _ in range(10):
    ground2 = copy.deepcopy(ground)
    for (y, row) in enumerate(ground):
        for (x, val) in enumerate(row):
            neighbors = ''.join([get(y+a, x+b) for (a, b) in
                                 [(-1, -1), (-1, 0), (-1, 1), (0, -1),
                                  (0, 1), (1, -1), (1, 0), (1, 1)]])
            if val == '.':
                if re.search('[|].*[|].*[|]', neighbors):
                    ground2[y][x] = '|'
            elif val == '|':
                if re.search('[#].*[#].*[#]', neighbors):
                    ground2[y][x] = '#'
            elif val == '#':
                if not re.search('[#].*[|]|[|].*[#]', neighbors):
                    ground2[y][x] = '.'
    ground = ground2
#    print('\n'.join(''.join(row) for row in ground))
#    print('')

    count1 = 0
    count2 = 0
    for (y, row) in enumerate(ground):
        for (x, val) in enumerate(row):
            if val == '|':
                count1 += 1
            elif val == '#':
                count2 += 1

    print((count1, count2))

print(count1 * count2)
