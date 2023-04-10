#!/usr/bin/env python3
import sys
import re

with open('aoc8.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

def parse_vm(lines):
    retval = []
    for line in lines:
        m = re.match(r'(nop|acc|jmp) \+?(-?\d+)', line)
        thing = m.group(1)
        val = int(m.group(2))
        retval.append((thing, val))
    return retval

def run_vm(code):
    visited = set()
    rip = 0
    acc = 0
    while rip not in visited and rip >= 0 and rip < len(code):
        visited.add(rip)
        (thing, val) = code[rip]
        if thing == 'acc':
            acc += val
            rip += 1
        elif thing == 'jmp':
            rip += val
        else:
            rip += 1
    return (rip, acc, rip in visited, visited)

# partA
vm1 = parse_vm(data)
(_, ans, is_loop, myvisited) = run_vm(vm1)
if not is_loop:
    raise Exception("Part A inconsistent")
print(ans)

for badinstr in reversed(range(len(data))):
    if badinstr in myvisited:
        (thing, val) = vm1[badinstr]
        if thing not in ('nop', 'jmp'):
            continue
        vm2 = list(vm1)
        vm2[badinstr] = ({'nop': 'jmp', 'jmp': 'nop'}[thing], val)
        (final_rip, ans, is_loop, _) = run_vm(vm2)
        if is_loop or final_rip < 0:
            continue
        print(ans)
        break
