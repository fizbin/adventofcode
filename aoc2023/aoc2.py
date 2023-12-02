#!/usr/bin/env python

from aoc_util import get_data_lines
import re


def parse_hand(s):
    m1 = re.search(r"(\d+) red", s)
    m2 = re.search(r"(\d+) blue", s)
    m3 = re.search(r"(\d+) green", s)
    rc, bc, gc = 0, 0, 0
    if m1:
        rc = int(m1.group(1))
    if m2:
        bc = int(m2.group(1))
    if m3:
        gc = int(m3.group(1))
    return (rc, bc, gc)


def parse_game(s):
    m = re.match(r"Game (\d+): (.*)", s)
    game_id = int(m.group(1))
    gameplay = m.group(2)
    rc, bc, gc = 0, 0, 0
    for hand in gameplay.split(";"):
        hparse = parse_hand(hand)
        rc = max(rc, hparse[0])
        bc = max(bc, hparse[1])
        gc = max(gc, hparse[2])
    return (game_id, rc, bc, gc)


data = get_data_lines(2)

game_restrictions = (12, 14, 13)

p1_ret = 0
p2_ret = 0
for game_s in data:
    (game_id, rc, bc, gc) = parse_game(game_s)
    if rc <= game_restrictions[0] and bc <= game_restrictions[1] and gc <= game_restrictions[2]:
        p1_ret += game_id
    p2_ret += rc * bc * gc
print(p1_ret)
print(p2_ret)
