"""
Advent of code 2021 day 22 in python. (Second alternate solution)

This solution is based on the inclusion/exclusion principle, with
the added insight that "turn off lights in region C" is equivalent to
the instruction "turn on lights in region C, then turn off lights in
region C".

Therefore, we can convert this algorithm for applying actions that
works only when all the commands are "on"

    def apply_action_on_only(onoff, control_block, blocks):
        assert(onoff, "This function only works for on commands")
        retval = []
        for (m, block) in blocks:
            inter = intersection(control_block, block)
            if inter is not None:
                retval.append((-m, inter))
        retval += blocks
        retval.append((1, control_block))
        return retval

Into one that works for "off" commands as well by simply not including
the control_block itself when the command is "off".
"""

import sys
import re


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


def intersection(cube1, cube2):
    if disjoint(cube1, cube2):
        return None
    return tuple((max(a, c), min(b, d)) for ((a, b), (c, d)) in zip(cube1, cube2))


def apply_action(onoff, control_block, blocks):
    retval = []
    for (m, block) in blocks:
        inter = intersection(control_block, block)
        if inter is not None:
            retval.append((-m, inter))
    retval += blocks
    if onoff:
        retval.append((1, control_block))
    return retval


def volume(cuboid):
    (xb, yb, zb) = cuboid
    return (xb[1] - xb[0]) * (yb[1] - yb[0]) * (zb[1] - zb[0])


def subset(cube1, cube2):
    (xb1, yb1, zb1) = cube1
    (xb2, yb2, zb2) = cube2
    return (
        xb2[0] <= xb1[0] < xb1[1] <= xb2[1]
        and yb2[0] <= yb1[0] < yb1[1] <= yb2[1]
        and zb2[0] <= zb1[0] < zb1[1] <= zb2[1]
    )


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
    print(sum(m * volume(block) for (m, block) in blocks))
    blocks = []
    for act in acts:
        blocks = apply_action(act[0], act[1], blocks)
    print(sum(m * volume(block) for (m, block) in blocks))
