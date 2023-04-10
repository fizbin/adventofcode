#!/usr/bin/env python3
import sys
import functools
import itertools
import re

def count_bits(bignum):
    return bin(bignum).count("1")

def fuzzsize(i1):
    if i1 is None:
        return 0
    return (1<<count_bits(i1[0]))

def intersect(i1, i2):
    if i1 is None or i2 is None:
        return None
    (and1, or1) = i1
    (and2, or2) = i2
    if (or1 & (~and1) & (~and2)) != (or2 & (~and1) & (~and2)):
        return None
    return (and1 & and2, or1 | or2)

def is_superset(i1, i2):
    return intersect(i1, i2) == i2

def doit():
    with open('aoc14.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)

    instructions = []
    maskand = 2**36 - 1
    maskor = 0
    for line in data:
        m = re.match('mask = ([01X]*)$', line)
        if m:
            maskand = int(m.group(1).replace('1', '0').replace('X', '1'), 2)
            maskor = int(m.group(1).replace('X', '0'), 2)
        else:
            m = re.match(r'mem\[(\d+)\] = (\d+)$', line)
            memloc = int(m.group(1))
            memval = int(m.group(2))
            memloc = memloc & (~ maskand)
            memloc = memloc | maskor
            instructions.append(((maskand, memloc), memval))

    print(len(instructions))
    new_instrs = []
    # now go through each instr and add that instr and the intersection with
    # each previous instr
    for i1, val1 in instructions:
        add_set = set([i1])
        for i2, _ in new_instrs:
            inter = intersect(i1, i2)
            if inter != None:
                add_set.add(inter)
        for i2 in sorted(add_set, reverse=True, key=lambda i: (fuzzsize(i), i)):
            new_instrs.append((i2, val1))
    print(len(new_instrs))
    # Now optimize by removing equal instructions from earlier in the list:
    instructions = []
    seen = set()
    for i1, val1 in reversed(new_instrs):
        if i1 not in seen:
            instructions.append((i1, val1))
            seen.add(i1)
    instructions.reverse()
    print(len(instructions))
    subset_record = set()
    for idx, (i1, _) in enumerate(instructions):
        for (i2, _) in instructions[idx+1:]:
            if (i1[0] & i2[0] == i2[0]) and is_superset(i1, i2):
                subset_record.add((i2, i1))
            if (i1[0] & i2[0] == i1[0]) and is_superset(i2, i1):
                subset_record.add((i1, i2))
    print('-----')
    # Now determine for each instruction the number of things actually
    # afected by that instruction by walking backwards
    backwards_instructions = []
    for indx, (i1, val1) in enumerate(reversed(instructions)):
        try:
            size = fuzzsize(i1)
            for (i2, _, num) in backwards_instructions:
                if (i2, i1) in subset_record:
                    size -= num
                    if size == 0:
                        break
            else:
                backwards_instructions.append((i1, val1, size))
        except:
            print("at index", indx)
            raise
    print(len(backwards_instructions))

    mysum = sum(val * num for (_, val, num) in backwards_instructions)
        
    print(mysum)

if __name__ == '__main__':
    doit()
    # print()
