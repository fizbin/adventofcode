"""
Advent of code 2021 day 19 in python.
"""

import sys
import re
import collections
import functools
import itertools

ROTATIONS = sum(
    (
        [(x, y, z), (-x, -y, z), (-x, y, -z), (x, -y, -z)]
        for (x, y, z) in [(1, 2, 3), (3, 1, 2), (2, 3, 1)]
    ),
    [],
) + sum(
    (
        [(-x, -y, -z), (x, y, -z), (x, -y, z), (-x, y, z)]
        for (x, y, z) in [(2, 1, 3), (1, 3, 2), (3, 2, 1)]
    ),
    [],
)


def find_pairwise_diffs(datapoints):
    listret = []
    for (pta, ptb) in itertools.combinations(datapoints, 2):
        listret.append((pta[0] - ptb[0], pta[1] - ptb[1], pta[2] - ptb[2]))
        listret.append((ptb[0] - pta[0], ptb[1] - pta[1], ptb[2] - pta[2]))
    return listret


def match_to_collective(
    collective_points: set, collective_diffs: set, scannerdat: list
):
    # returns None or new scanner data. Updates collective_diffs in place
    scanner_diffs = find_pairwise_diffs(scannerdat)
    for rotation in ROTATIONS:
        rotated = apply_rotation(rotation, scanner_diffs)
        common_len = len(collective_diffs & set(rotated))
        if common_len > 2:
            intersecting_diffs = collective_diffs & set(rotated)
            # found rotation!
            # now, alignment
            goal_size = len(collective_points) + len(scannerdat) - 12
            rotdat = apply_rotation(rotation, scannerdat)
            for (pta, ptb) in itertools.combinations(rotdat, 2):
                diff = (pta[0] - ptb[0], pta[1] - ptb[1], pta[2] - ptb[2])
                if diff in intersecting_diffs:
                    for pt in collective_points:
                        move = (pt[0] - pta[0], pt[1] - pta[1], pt[2] - pta[2])
                        if (
                            len(collective_points | set(apply_move(move, rotdat)))
                            <= goal_size
                        ):
                            retval = apply_move(move, rotdat)
                            collective_diffs.update(rotated)
                            collective_points.update(retval)
                            return (move, rotation, retval)
    return (None, None, None)


def apply_move(move, datapoints):
    return [
        (tup[0] + move[0], tup[1] + move[1], tup[2] + move[2]) for tup in datapoints
    ]


def apply_rotation(rot, datapoints):
    if rot == (1, 2, 3):
        return datapoints
    listret = []
    for datapoint in datapoints:
        retval = [
            datapoint[abs(rot[0]) - 1],
            datapoint[abs(rot[1]) - 1],
            datapoint[abs(rot[2]) - 1],
        ]
        if rot[0] < 0:
            retval[0] *= -1
        if rot[1] < 0:
            retval[1] *= -1
        if rot[2] < 0:
            retval[2] *= -1
        listret.append(tuple(retval))
    return listret


def reverse_rotation(rot):
    for trot in ROTATIONS:
        if apply_rotation(trot, [rot]) == [(1, 2, 3)]:
            return trot
    raise Exception("WTF?")


def reverse_move(mov):
    return (-mov[0], -mov[1], -mov[2])


if __name__ == "__main__":
    with open("aoc19.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
        # data = re.findall(r"(\w+)-(\w+)", f.read())
        # data = re.findall(r"(-?\d+)\.\.(-?\d+)", f.read())
        data = re.findall(r"scanner [^s]*", f.read())
        # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
        # data = [tuple(int(y) for y in tup) for tup in data]
        scannerstrs = [re.findall(r"([\d-]+),([\d-]+),([\d-]+)", sstr) for sstr in data]
        scannerdat = [
            [(int(tup[0]), int(tup[1]), int(tup[2])) for tup in scannerstrlist]
            for scannerstrlist in scannerstrs
        ]

    remaining_scanners = scannerdat
    collective_points = set(scannerdat[0])
    collective_diff = set(find_pairwise_diffs(scannerdat[0]))

    collected = set([0])
    scanner_locs = set([(0, 0, 0)])
    while len(collected) < len(remaining_scanners):
        l1 = len(collected)
        for (idx, scanner) in list(enumerate(remaining_scanners)):
            if idx in collected:
                continue
            (move, rotation, found) = match_to_collective(
                collective_points, collective_diff, scanner
            )
            if found is not None:
                # print(f"Matched on {idx}! {len(remaining_scanners)} ({len(scanner)} -> {len(found)}, now {len(collective_points)}")
                # old_loc = apply_rotation(reverse_rotation(rotation), apply_move(reverse_move(move), [(26,-1119,1091)]))
                # print(f"Maybe old loc {old_loc}")
                collected.add(idx)
                scanner_locs.add(move)
        if l1 == len(collected):
            print("FAIL!")
            break
    print(len(collective_points))
    for beacon in sorted(collective_points):
        pass  # print(beacon)

    maxdist = 0
    for (scan1, scan2) in itertools.combinations(scanner_locs, 2):
        dist = (
            abs(scan1[0] - scan2[0])
            + abs(scan1[1] - scan2[1])
            + abs(scan1[2] - scan2[2])
        )
        maxdist = max([dist, maxdist])
    print(maxdist)
