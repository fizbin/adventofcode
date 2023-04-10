#!/usr/bin/env python3
import sys
import re

with open('aoc8.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

visited = set()
rip = 0
acc = 0
while rip not in visited:
    visited.add(rip)
    instr = data[rip]
    m = re.match(r'(nop|acc|jmp) \+?(-?\d+)', instr)
    thing = m.group(1)
    val = int(m.group(2))
    if thing == 'acc':
        acc += val
        rip += 1
    elif thing == 'jmp':
        rip += val
    else:
        rip += 1
print(acc)

tricky = 0
while True:
    trick = tricky
    data1 = list(data)
    for badinstr in reversed(range(len(data))):
        line = data1[badinstr]
        if badinstr in visited:
            if line.startswith('jmp'):
                if trick > 0:
                    trick -= 1
                    print('at', badinstr)
                else:
                    data1[badinstr] = 'nop' + data1[badinstr][3:]
                    print('flip', badinstr, 'to', data[badinstr])
                    break
            elif line.startswith('nop'):
                m = re.match(r'(nop|acc|jmp) \+?(-?\d+)', line)
                val = int(m.group(2))
                if trick > 0:
                    trick -= 1
                    print('at', badinstr)
                else:
                    data1[badinstr] = 'jmp' + data1[badinstr][3:]
                    print('flip', badinstr, 'to', data[badinstr])
                    break


    print('modified...')
    rip = 0
    acc = 0
    nvisited = set()
    won = False
    try:
        while rip not in nvisited:
            if rip < 0:
                break
            if rip >= len(data1):
                print(acc)
                won = True
                break
            nvisited.add(rip)
            instr = data1[rip]
            m = re.match('(nop|acc|jmp) \+?(-?\d+)', instr)
            thing = m.group(1)
            val = int(m.group(2))
            if thing == 'acc':
                acc += val
                rip += 1
            elif thing == 'jmp':
                rip += val
            else:
                rip += 1
    except IndexError:
        print(rip)
        raise
    if won:
        break
    tricky += 1
