import numpy as np
import re
import copy

with open('aoc13.in.txt') as f:
    data = list(list(re.match(r'^.*', ln).group(0)) for ln in f)

track = copy.deepcopy(data)
carts = []
moves = {
    '^': (0, -1),
    '>': (1, 0),
    '<': (-1, 0),
    'v': (0, 1),
}

slash = {
    '^': '>',
    '>': '^',
    '<': 'v',
    'v': '<',
    }
backslash = {
    '^': '<',
    '>': 'v',
    '<': '^',
    'v': '>',
    }


def turn(turn_num, current):
    dirs = '^>v<'
    dirn = dirs.index(current)
    new_dirn = (dirn + 3 + (turn_num % 3)) % 4
    return dirs[new_dirn:new_dirn+1]


for y in range(len(track)):
    for x in range(len(track[y])):
        here = track[y][x]
        if here in '^><v':
            carts.append((y, x, here, 0))
            track[y][x] = '|-|-'['^>v<'.index(here)]

cart_locs = set((x, y) for (y, x, _, _) in carts)

def runit():
    tick = 0
    while True:
        tick += 1
        for (cart, cart_no) in sorted((c, n) for (n, c) in enumerate(carts)):
            (y, x, here, turn_no) = cart
            (mov_x, mov_y) = moves[here]
            newy = y + mov_y
            newx = x + mov_x
            if (newx, newy) in cart_locs:
                print("Crash at %r while working on tick %d" % ((newx, newy),tick))
                return
            cart_locs.remove((x, y))
            cart_locs.add((newx, newy))
            newt = track[newy][newx]
            nturn_no = turn_no
            nhere = here
            if newt in '+/\\':
                if newt == '+':
                    nhere = turn(turn_no, here)
                    nturn_no = turn_no + 1
                elif newt == '/':
                    nhere = slash[here]
                elif newt == '\\':
                    nhere = backslash[here]
            carts[cart_no] = (newy, newx, nhere, nturn_no)


runit()
