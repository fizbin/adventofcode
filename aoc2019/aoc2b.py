#!/usr/bin/env python
import sys
import copy

with open('aoc2a.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

for line in data:
    prog = [int(x) for x in line.split(',')]


def evalprog(iprog, param1, param2):
    prog = list(iprog)
    idx = 0
    prog[1] = param1
    prog[2] = param2
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
    return prog[0]


def findval():
    for i1 in range(99):
        for i2 in range(99):
            if evalprog(prog, i1, i2) == 19690720:
                print(100*i1+i2)
                return


findval()
