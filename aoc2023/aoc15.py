#!/usr/bin/python
import aoc_util

data = aoc_util.get_data(15)
data = data.replace("\n", "")


def hash(mstr):
    mbyt = mstr.encode("ascii")
    ret = 0
    for n in mbyt:
        ret += n
        ret *= 17
        ret %= 256
    return ret


total = 0
for instr in data.split(","):
    # print(f"{instr} -> {hash(instr)}")
    total += hash(instr)
print(total)

boxes = [[] for _ in range(256)]
for instr in data.split(","):
    if instr.endswith("-"):
        lbl = instr[:-1]
        hlbl = hash(lbl)
        boxes[hlbl] = [x for x in boxes[hlbl] if x[0] != lbl]
    else:
        (lbl, vals) = instr.split("=")
        val = int(vals)
        hlbl = hash(lbl)
        if any(x[0] == lbl for x in boxes[hlbl]):
            boxes[hlbl] = [
                (x, newy) for (x, y) in boxes[hlbl] for newy in ([val] if x == lbl else [y])
            ]
        else:
            boxes[hlbl].append((lbl, val))

total = 0
for idx, box in enumerate(boxes):
    idxf = idx + 1
    for idx2, (lbl, val) in enumerate(box):
        idxf2 = idx2 + 1
        total += idxf * idxf2 * val
print(total)
