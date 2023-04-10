#!/usr/bin/env python3
import sys
import re

with open('aoc11.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

diagram = [row for row in data]

def surrounding(row, col):
    occ_total = 0
    for rowd in (-1, 0, 1):
        for cold in (-1, 0, 1):
            if (rowd == cold == 0):
                continue
            if row + rowd < 0 or row + rowd >= len(diagram):
                continue
            if col + cold < 0 or col + cold >= len(diagram[0]):
                continue
            if diagram[row+rowd][col+cold] == '#':
                occ_total += 1
    return occ_total

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
                if surrounding(r, c) >= 4:
                    new_row += 'L'
                else:
                    new_row += '#'
        new_d.append(new_row)
    done = (''.join(new_d) == ''.join(diagram))

print('\n'.join(diagram))
print(len(re.findall('#', ''.join(diagram))))
