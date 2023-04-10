#!/usr/bin/env python
import sys

with open('aoc2a.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

for line in data:
    prog = [int(x) for x in line.split(',')]

    idx = 0
    prog[1] = 12
    prog[2] = 2
    while True:
        if prog[idx] == 1:
            prog[prog[idx+3]] = prog[prog[idx+1]] + prog[prog[idx+2]]
            idx += 4
        elif prog[idx] == 2:
            prog[prog[idx+3]] = prog[prog[idx+1]] * prog[prog[idx+2]]
            idx += 4
        elif prog[idx] == 99:
            idx += 1
            break
        else:
            print("BAD CODE AT %d" % idx)
            sys.exit(2)

    print(prog[0])
