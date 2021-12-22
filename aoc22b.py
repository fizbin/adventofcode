"""
Advent of code 2021 day 22 in python.
"""

import sys
import re


def split(axis, lim, cuboid):
    if cuboid[axis][0] < lim < cuboid[axis][1]:
        cube1 = list(cuboid)
        cube2 = list(cuboid)
        cube1[axis] = (cuboid[axis][0], lim)
        cube2[axis] = (lim, cuboid[axis][1])
        return (tuple(cube1), tuple(cube2))
    else:
        raise Exception(f"Bad split {axis} {lim} {cuboid}")


def disjoint(cube1, cube2):
    (xb1, yb1, zb1) = cube1
    (xb2, yb2, zb2) = cube2
    if xb1[1] <= xb2[0] or xb2[1] <= xb1[0]:
        return True
    if yb1[1] <= yb2[0] or yb2[1] <= yb1[0]:
        return True
    if zb1[1] <= zb2[0] or zb2[1] <= zb1[0]:
        return True
    return False


def subset(cube1, cube2):
    (xb1, yb1, zb1) = cube1
    (xb2, yb2, zb2) = cube2
    return (
        xb2[0] <= xb1[0] < xb1[1] <= xb2[1]
        and yb2[0] <= yb1[0] < yb1[1] <= yb2[1]
        and zb2[0] <= zb1[0] < zb1[1] <= zb2[1]
    )


def apply_action(onoff, control_block, blocks):
    retval = []
    working_blocks = list(blocks)
    while working_blocks:
        block = working_blocks.pop(0)
        if disjoint(control_block, block):
            retval.append(block)
            continue
        if subset(block, control_block):
            continue
        if onoff and subset(control_block, block):
            # shortcut!
            return retval + [block] + working_blocks
        # Okay, so "block" is not a subset of control_block
        # and that means that some direction must stick out
        # further than control_block
        for axis in (0, 1, 2):
            if block[axis][0] < control_block[axis][0]:
                (b1, b2) = split(axis, control_block[axis][0], block)
                working_blocks[0:0] = [b1, b2]
                break
            if block[axis][1] > control_block[axis][1]:
                (b1, b2) = split(axis, control_block[axis][1], block)
                working_blocks[0:0] = [b1, b2]
                break
        else:
            # python's for...else, meaning "if the for loop never hit a break"
            raise Exception(f"Unable to split block {block} control {control_block}")

    if onoff:
        retval.append(control_block)
    return retval


def volume(cuboid):
    (xb, yb, zb) = cuboid
    return (xb[1] - xb[0]) * (yb[1] - yb[0]) * (zb[1] - zb[0])


if __name__ == "__main__":
    with open("aoc22.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        data = re.findall(
            r"(on|off) x=([-\d]+)\.\.([-\d]+),y=([-\d]+)\.\.([-\d]+),z=([-\d]+)\.\.([-\d]+)",
            f.read(),
        )
        # Use +1 convention to be like python's [a:b] notation
        acts = [
            (
                t[0] == "on",
                (
                    (int(t[1]), int(t[2]) + 1),
                    (int(t[3]), int(t[4]) + 1),
                    (int(t[5]), int(t[6]) + 1),
                ),
            )
            for t in data
        ]
    small_acts = [
        act for act in acts if subset(act[1], ((-50, 50), (-50, 50), (-50, 50)))
    ]
    blocks = []
    for act in small_acts:
        blocks = apply_action(act[0], act[1], blocks)
    print(sum(volume(block) for block in blocks))
    blocks = []
    for act in acts:
        blocks = apply_action(act[0], act[1], blocks)
    print(sum(volume(block) for block in blocks))
