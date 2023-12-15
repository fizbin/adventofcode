#!/usr/bin/python

import aoc_util
import hashlib
import sys

data = aoc_util.get_data(14)
grid = aoc_util.chargrid(data)


def roll_north(coldata: list[str]):
    n_rounds = 0
    n_empties = 0
    sofar = []
    for pos in range(len(coldata)):
        if coldata[pos] == "O":
            n_rounds += 1
        elif coldata[pos] == ".":
            n_empties += 1
        elif coldata[pos] == "#":
            sofar += ["O"] * n_rounds + ["."] * n_empties + ["#"]
            n_rounds, n_empties = 0, 0
    sofar += ["O"] * n_rounds + ["."] * n_empties
    return sofar


def calc_load(coldata: list[str]):
    return sum((len(coldata) - idx) for (idx, ch) in enumerate(coldata) if ch == "O")


total = 0
for colpos in range(len(grid[0])):
    coldata = [row[colpos] for row in grid]
    total += calc_load(roll_north(coldata))
print(total)


def roll_cycle(coldata):
    working0 = coldata
    working1 = []

    # this operation is "north, then transpose" or equiv. "transpose, west"
    for colpos in range(len(working0[0])):
        coldata = [row[colpos] for row in working0]
        working1.append(roll_north(coldata))
    working0 = working1
    working1 = []

    # repeat the same operation
    for colpos in range(len(working0[0])):
        coldata = [row[colpos] for row in working0]
        working1.append(roll_north(coldata))
    working0 = working1
    working1 = []
    # the two "transpose"s cancel, so we've effectively done "north, then west"

    # this operation is "transpose, then east"; equiv. "south, then transpose"
    for colpos in range(len(working0[0])):
        coldata = [row[colpos] for row in working0]
        working1.append(list(reversed(roll_north(list(reversed(coldata))))))
    working0 = working1
    working1 = []

    # and repeat
    for colpos in range(len(working0[0])):
        coldata = [row[colpos] for row in working0]
        working1.append(list(reversed(roll_north(list(reversed(coldata))))))

    # so now the equiv. to what we've done is "north, west, south, east"
    return working1


def get_signature(grid):
    return hashlib.sha256("\n".join("".join(row) for row in grid).encode("ascii")).hexdigest()


cycle_sigs = {}
c_num = 0
while True:
    grid = roll_cycle(grid)
    sig = get_signature(grid)
    c_num += 1
    if sig in cycle_sigs:
        # print(f"FOUND! {c_num} is same as {cycle_sigs[sig]}")
        break
    cycle_sigs[sig] = c_num
    if c_num % 1000 == 0:
        print(c_num)
        # print('\n'.join(''.join(row) for row in grid))
        # print()
equivalent = (1000000000 - c_num) % (c_num - cycle_sigs[sig])
for _ in range(equivalent):
    grid = roll_cycle(grid)
total = 0
for colpos in range(len(grid[0])):
    coldata = [row[colpos] for row in grid]
    total += calc_load(coldata)
print(total)
