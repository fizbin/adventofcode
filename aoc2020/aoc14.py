#!/usr/bin/env python3
import sys
import functools
import re


def doit():
    with open('aoc14.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)

    memory = {}
    maskand = 2**36 - 1
    maskor = 0
    for line in data:
        m = re.match('mask = ([01X]*)$', line)
        if m:
            maskand = re.sub('1', '0', m.group(1))
            maskand = re.sub('X', '1', maskand)
            maskand = int(maskand, 2)
            maskor = int(re.sub('X', '0', m.group(1)), 2)
        else:
            m = re.match(r'mem\[(\d+)\] = (\d+)$', line)
            memory[int(m.group(1))] = (int(m.group(2)) & maskand) | maskor
    print(sum(memory.values()))

if __name__ == '__main__':
    doit()
