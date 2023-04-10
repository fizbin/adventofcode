#!/usr/bin/env python3
import sys
import time

with open('aoc3.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(line.strip() for line in f)

tree = '#'[0]
width = len(data[0])
height = len(data)

def find_encounters(step):
    retval = 0
    coords = 0j
    while coords.imag < height:
        while coords.real >= width:
            coords -= width
        if data[int(coords.imag)][int(coords.real)] == tree:
            retval += 1
        coords += step
    return retval

start = time.time()
print(find_encounters(3+1j))

print()

print(find_encounters(1+1j) * find_encounters(3+1j) *
      find_encounters(5+1j) * find_encounters(7+1j) *
      find_encounters(1+2j))
print(time.time() - start)
