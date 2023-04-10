"""
Advent of code 2021 day 24 in python.
"""

import sys
import re
from math import prod
import random


def run_inst(state, inplist, n, ins):
    vals = []
    for x in ins[1:]:
        if re.match(r"-?\d+", x):
            vals.append(int(x))
        elif x in ("w", "x", "y", "z"):
            vals.append(state["wxyz".index(x)])
        else:
            raise Exception(f"Bad ins: {ins}")
    # inp add mul div mod eql
    code = ins[0]
    if code == "inp":
        res = inplist.pop(0)
    elif code == "add":
        res = sum(vals)
    elif code == "mul":
        res = prod(vals)
    elif code == "div":
        res = abs(vals[0]) // abs(vals[1])
        if prod(vals) < 0:
            res = -res
    elif code == "mod":
        res = vals[0] % vals[1]
    elif code == "eql":
        res = int(vals[0] == vals[1])
    state["wxyz".index(ins[1])] = res
    if n > 0:
        print(f"{n}: {state}")


if __name__ == "__main__":
    with open("aoc24.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        data = re.findall(
            r"[\S ]+",
            f.read(),
        )
        inlist = [tuple(re.findall(r"\S+", line)) for line in data]

    printedFirst = False
    lastValid = None
    for x0 in range(1, 10):
        for x1 in range(1, 10):
            for x5 in range(1, 10):
                for x6 in range(1, 10):
                    for x7 in range(1, 10):
                        for x11 in range(1, 10):
                            inplist = [0] * 14
                            # That I only need six digits and how to handle the rest of the digits was
                            # determined by reverse engineering my problem input. This will likely
                            # not work on your problem input.
                            inplist[1] = x1
                            inplist[5] = x5
                            inplist[6] = x6
                            inplist[7] = x7
                            inplist[11] = x11
                            inplist[0] = x0
                            inplist[2] = 1
                            inplist[3] = 9
                            inplist[4] = inplist[1] - 3
                            inplist[8] = inplist[7] + 2
                            inplist[9] = inplist[6] - 1
                            inplist[10] = inplist[5] - 4
                            inplist[12] = inplist[11] - 5
                            inplist[13] = inplist[0] - 7
                            if any(x < 1 or x > 9 for x in inplist):
                                continue
                            for j in range(len(inplist)):
                                inplist[j] = min(9, max(1, inplist[j]))
                            sinp = "".join(str(x) for x in inplist)
                            state = [0, 0, 0, 0]
                            for (n, ins) in enumerate(inlist):
                                run_inst(state, inplist, 0, ins)
                            if state[3] == 0:
                                lastValid = sinp
                                if not printedFirst:
                                    print(sinp)
                                    printedFirst = True
    print(lastValid)
