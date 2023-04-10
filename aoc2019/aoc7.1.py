#!/usr/bin/python
import sys
import re
import collections
import math
import itertools
import cmath
import queue

with open('aoc7.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)


for line in data:
    prog = [int(x) for x in line.split(',')]


def get_param(prog, imode, val):
    if imode % 10 == 0:
        return prog[val]
    if imode % 10 == 1:
        return val
    print("BAD get imode %d" % (imode,))
    sys.exit(2)


def set_param(prog, imode, val, set_to):
    if imode % 10 == 0:
        prog[val] = set_to
        return
    print("BAD set imode %d" % (imode,))
    sys.exit(2)


def helping_debug(prog, idx):
    return (prog[223:227], prog[677], idx)
    pprog = prog + [-1, -1, -1, -1, -1, -1]
    h = [prog[idx] % 100]
    if (prog[idx] // 100) % 10 == 0:
        h.append(pprog[idx+1])
    else:
        h.append(idx+1)
    if (prog[idx] // 1000) % 10 == 0:
        h.append(pprog[idx+2])
    else:
        h.append(idx+2)
    if (prog[idx] // 10000) % 10 == 0:
        h.append(pprog[idx+3])
    else:
        h.append(idx+3)
    return h


def evalprog(iprog, inputf, outputf):
    prog = list(iprog)
    idx = 0
    while True:
        #print("DBG: %r %r" % (helping_debug(prog, idx),prog[idx:idx+5]))
        if prog[idx] % 100 == 1:
            set_param(prog, prog[idx] // 10000,
                      prog[idx+3],
                      get_param(prog, prog[idx] // 100, prog[idx+1])
                      +
                      get_param(prog, prog[idx] // 1000, prog[idx+2]))
            idx += 4
        elif prog[idx] % 100 == 2:
            set_param(prog, prog[idx] // 10000,
                      prog[idx+3],
                      get_param(prog, prog[idx] // 100, prog[idx+1])
                      *
                      get_param(prog, prog[idx] // 1000, prog[idx+2]))
            idx += 4
        elif prog[idx] % 100 == 3:
            set_param(prog, prog[idx] // 100, prog[idx+1], inputf())
            idx += 2
        elif prog[idx] % 100 == 4:
            outputf(get_param(prog, prog[idx] // 100, prog[idx+1]))
            idx += 2
        elif prog[idx] % 100 == 5:
            p1 = get_param(prog, prog[idx] // 100, prog[idx+1])
            if p1:
                idx = get_param(prog, prog[idx] // 1000, prog[idx+2])
            else:
                idx += 3
        elif prog[idx] % 100 == 6:
            p1 = get_param(prog, prog[idx] // 100, prog[idx+1])
            if not p1:
                idx = get_param(prog, prog[idx] // 1000, prog[idx+2])
            else:
                idx += 3
        elif prog[idx] % 100 == 7:
            p1 = get_param(prog, prog[idx] // 100, prog[idx+1])
            p2 = get_param(prog, prog[idx] // 1000, prog[idx+2])
            set_param(prog, prog[idx] // 10000, prog[idx + 3],
                      1 if p1 < p2 else 0)
            idx += 4
        elif prog[idx] % 100 == 8:
            p1 = get_param(prog, prog[idx] // 100, prog[idx+1])
            p2 = get_param(prog, prog[idx] // 1000, prog[idx+2])
            set_param(prog, prog[idx] // 10000, prog[idx + 3],
                      1 if p1 == p2 else 0)
            print("p1 was %d, p2 was %d" % (p1, p2))
            idx += 4
        elif prog[idx] % 100 == 99:
            idx += 1
            break
        else:
            print("BAD CODE %d AT %d" % (prog[idx], idx))
            sys.exit(2)
    return prog[0]


max_output = -1000
for ordering in itertools.permutations([0, 1, 2, 3, 4]):
    queues = [queue.Queue() for _ in range(6)]
    for (ique, order) in zip(queues, ordering):
        ique.put(order)
    queues[0].put(0)
    for idx in range(5):
        evalprog(prog, queues[idx].get_nowait, queues[idx+1].put_nowait)
    max_output = max(max_output, queues[5].get_nowait())

print(max_output)

