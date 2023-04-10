#!/usr/bin/env python3
import sys

with open('20_input.txt' if len(sys.argv) < 2 else sys.argv[1], 'r') as f:
    data = f.read()


dir = {'N': 1j, 'S': -1j, 'E': 1, 'W': -1}

def traverse(it, map={0: 0}, pos=0, depth=0):
    initial_pos = pos
    initial_depth = depth
    
    for c in it:
        if c in dir:
            pos += dir[c]
            if pos in map:  # been here before, so we are backtracking
                depth = map[pos]
            else:
                depth += 1
                map[pos] = depth
        elif c == '|':
            pos = initial_pos
            depth = initial_depth
        elif c == '(':
            traverse(it, map, pos, depth)
        elif c == ')':
            return
        elif c == '$':
            return map
        else:
            print(f'Unknown character: {c}')


map = traverse(iter(data[1:]))
print('Part 1:', max(map.values()))
print('Part 2:', sum(1 for n in map.values() if n >= 1000))
