#!/usr/bin/env python3
import sys
import functools
import re

@functools.lru_cache(maxsize=None)
def flux(xbits, fluxbit):
#    print(xbits, fluxbit)
    if (fluxbit == 0):
        return []
    if not (xbits & (fluxbit - 1)):
        return [0, fluxbit]
    if not (fluxbit & xbits):
        return flux(xbits, fluxbit // 2)
    return sorted(
        sum(([fluxbit + other, other]
             for other in flux(xbits, fluxbit // 2)), []))

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
            memloc = int(m.group(1))
            memval = int(m.group(2))
            memloc = memloc & (~ maskand)
            memloc = memloc | maskor
            for fl in flux(maskand, 2**36):
                #print(maskand, fl, memloc | fl, memval)
                memory[memloc | fl] = memval

    print(len(memory.values()))
    print(sum(memory.values()))

if __name__ == '__main__':
    doit()
