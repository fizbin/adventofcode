#!/usr/bin/env python3
import sys
import random
import re
import functools

with open('aoc11.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

diagram = [row for row in data]

@functools.lru_cache(maxsize=None)
def nbhd(row, col):
    result = []
    for rowd in (-1, 0, 1):
        for cold in (-1, 0, 1):
            if (rowd == cold == 0):
                continue
            for mul in range(1, len(diagram)+2):
                if row + mul*rowd < 0 or row + mul*rowd >= len(diagram):
                    break
                if col + mul*cold < 0 or col + mul*cold >= len(diagram[0]):
                    break
                if diagram[row+mul*rowd][col+mul*cold] != '.':
                    result += [(row+mul*rowd, col+mul*cold)]
                    break
    return result

def surrounding(row, col):
    result = 0
    for (r, c) in nbhd(row, col):
        result += int(diagram[r][c] == '#')
    return result

#diagram = [re.sub('L', lambda m: random.choice('#LLLL'), x) for x in diagram]
done = False
new_d = diagram
while not done:
    diagram = new_d
    new_d = []
    for r, rowcont in enumerate(diagram):
        new_row = ''
        for c, ccont in enumerate(rowcont):
            if ccont == '.':
                new_row += '.'
            elif ccont == 'L':
                if surrounding(r, c) == 0:
                    new_row += '#'
                else:
                    new_row += 'L'
            elif ccont == '#':
                if surrounding(r, c) >= 5:
                    new_row += 'L'
                else:
                    new_row += '#'
        new_d.append(new_row)
    done = (''.join(new_d) == ''.join(diagram))


print('\n'.join(diagram))
print(len(re.findall('#', ''.join(diagram))))
