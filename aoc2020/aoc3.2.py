#!/usr/bin/env python3
import sys
import timeit


with open('aoc3.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(line.strip() for line in f)

tree = '#'[0]
width = len(data[0])
height = len(data)

data1 = []
for row in data:
    data1.append([int(char == tree) for char in row])

def find_encounters(over, down):
    retval = 0
    x = 0
    y = 0
    while y < height:
        x %= width
        retval += data1[y][x]
        x += over
        y += down
    return retval

print(find_encounters(3, 1))

print()

print(find_encounters(1, 1) * find_encounters(3, 1) *
      find_encounters(5, 1) * find_encounters(7, 1) *
      find_encounters(1, 2))


#print(timeit.repeat(lambda: find_encounters(3, 1), number=1000, repeat=40)[-1])
