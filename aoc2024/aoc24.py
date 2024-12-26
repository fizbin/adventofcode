import copy
import operator
import itertools

from aoc_util import get_data_paras

(init_val_spec, gates) = get_data_paras(24)

init_vals = {}
for line in init_val_spec.splitlines():
    (wire, valstr) = line.split(":")
    val = int(valstr)
    init_vals[wire] = val

notify: dict[str, list[str]] = {}
compute = {}
for line in gates.splitlines():
    # print(repr(line))
    (inA, op, inB, _, outW) = line.split()
    (inA, inB) = sorted([inA, inB])
    notify.setdefault(inA, []).append(outW)
    notify.setdefault(inB, []).append(outW)
    if op == "AND":
        compute[outW] = (operator.and_, inA, inB)
    elif op == "OR":
        compute[outW] = (operator.or_, inA, inB)
    elif op == "XOR":
        compute[outW] = (operator.xor, inA, inB)

working = set()
for i in init_vals:
    working.update(notify.get(i, []))

vals = copy.copy(init_vals)
while working:
    nworking = set()
    for w in working:
        if w in compute and compute[w][1] in vals and compute[w][2] in vals:
            vals[w] = compute[w][0](vals[compute[w][1]], vals[compute[w][2]])
            nworking.update(notify.get(w, []))
    working = nworking

znodevals = sorted((zn, zv) for (zn, zv) in vals.items() if zn.startswith("z"))
print("Part 1:", int("".join(str(x[1]) for x in reversed(znodevals)), 2))


def attempt_add(xval, yval):
    vals = {}
    working = set()
    for loc in range(45):
        vals[f"x{loc:02}"] = xval % 2
        vals[f"y{loc:02}"] = yval % 2
        xval //= 2
        yval //= 2
        working.update(notify.get(f"x{loc:02}", []))
        working.update(notify.get(f"y{loc:02}", []))
    round = 0
    while working:
        round += 1
        nworking = set()
        for w in working:
            if w in compute and compute[w][1] in vals and compute[w][2] in vals:
                if w not in vals:
                    vals[w] = compute[w][0](vals[compute[w][1]], vals[compute[w][2]])
                    nworking.update(notify.get(w, []))
        working = nworking
    znodevals = sorted((zn, zv) for (zn, zv) in vals.items() if zn.startswith("z"))
    return int("".join(str(x[1]) for x in reversed(znodevals)), 2)


def do_flip(wireA, wireB):
    compute[wireA], compute[wireB] = compute[wireB], compute[wireA]

    notify.setdefault(compute[wireB][1], []).remove(wireA)
    notify.setdefault(compute[wireB][2], []).remove(wireA)
    notify.setdefault(compute[wireA][1], []).remove(wireB)
    notify.setdefault(compute[wireA][2], []).remove(wireB)

    notify.setdefault(compute[wireA][1], []).append(wireA)
    notify.setdefault(compute[wireA][2], []).append(wireA)
    notify.setdefault(compute[wireB][1], []).append(wireB)
    notify.setdefault(compute[wireB][2], []).append(wireB)


def find_connected(seeds, levels=6):
    working = set(seeds)
    accum = set(seeds)
    while levels > 0:
        levels -= 1
        nworking = set()
        for w in working:
            if w in compute:
                nworking.add(compute[w][1])
                nworking.add(compute[w][2])
            if w in notify:
                nworking.update(notify[w])
        nworking.difference_update(accum)
        working = nworking
        accum.update(working)
    return accum


def find_good_flip(testx, testy, wires):
    good_flips = []
    for flip_args in itertools.combinations(wires, 2):
        if flip_args[0] not in compute or flip_args[1] not in compute:
            continue
        do_flip(*flip_args)
        if all(x + y == attempt_add(x, y) for x in testx for y in testy):
            good_flips.append(flip_args)
        do_flip(*flip_args)
    if len(good_flips) != 1:
        print("ERROR: ", good_flips)
        exit(1)
    return good_flips[0]


flippers = set()
for i in range(45):
    xval = 1 << i
    something_to_see = False
    if attempt_add(xval, 0) != xval:
        testrange = [x * (1 << (i - 1)) for x in range(8)]
        flip = find_good_flip(
            testrange, testrange, find_connected([f"x{i:02}", f"y{i:02}", f"z{i:02}"])
        )
        # print("FLIP:", flip)
        do_flip(*flip)
        flippers.update(flip)
    if attempt_add(0, xval) != xval:
        testrange = [x * (1 << (i - 1)) for x in range(8)]
        flip = find_good_flip(
            testrange, testrange, find_connected([f"x{i:02}", f"y{i:02}", f"z{i:02}"])
        )
        # print("FLIP:", flip)
        do_flip(*flip)
        flippers.update(flip)
    if attempt_add(xval, xval) != 2 * xval:
        testrange = [x * (1 << (i - 1)) for x in range(16)]
        flip = find_good_flip(
            testrange, testrange, find_connected([f"x{i:02}", f"y{i:02}", f"z{i+1:02}"])
        )
        # print("FLIP:", flip)
        do_flip(*flip)
        flippers.update(flip)


# print("RETEST!")

for i in range(45):
    xval = 1 << i
    something_to_see = False
    if attempt_add(xval, 0) != xval:
        print(f"Still bad", xval, 0, xval+0, attempt_add(xval, 0))
    if attempt_add(0, xval) != xval:
        print(f"Still bad", 0, xval, xval+0, attempt_add(0,xval))
    if attempt_add(xval, xval) != 2 * xval:
        print(f"Still bad", xval, xval, xval+xval, attempt_add(xval,xval))

print("Part 2:", ",".join(sorted(flippers)))
