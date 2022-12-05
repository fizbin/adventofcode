#!/usr/bin/env python

from aoc_util import get_data_paras, numbers, chargrid
import copy

(stacks, orders) = get_data_paras(5)
stacks = chargrid(stacks)

real_stacks = [[] for _ in range(1,10)]
for row in range(8):
    for col in range(9):
        ch = stacks[row][1+4*col]
        if ch != ' ':
            real_stacks[col].append(ch)

oreal_stacks = copy.deepcopy(real_stacks)

orders = orders.splitlines()
for order in orders:
    (n, von, zu) = numbers(order)
    moved = real_stacks[von-1][0:n]
    real_stacks[von-1][0:n] = []
    real_stacks[zu-1] = list(reversed(moved)) + real_stacks[zu-1]

print(''.join(x[0] if x else '' for x in real_stacks))

real_stacks = oreal_stacks

for order in orders:
    (n, von, zu) = numbers(order)
    moved = real_stacks[von-1][0:n]
    real_stacks[von-1][0:n] = []
    real_stacks[zu-1] = moved + real_stacks[zu-1]
