from aoc_util import *
import re
import itertools
import math
from typing import Tuple, List
from dataclasses import dataclass


def part1(grid, moves):
    jumps = {}
    space = {}
    for row in range(len(grid)):
        mnc = min(col for col in range(len(grid[row])) if grid[row][col] != " ")
        mxc = max(col for col in range(len(grid[row])) if grid[row][col] != " ")
        jumps[row + (mnc - 1) * 1j] = row + mxc * 1j
        jumps[row + (mxc + 1) * 1j] = row + mnc * 1j
        for col in range(len(grid[row])):
            space[row + col * 1j] = grid[row][col]
    for col in range(len(grid[0])):
        mnr = min(row for row in range(len(grid)) if col < len(grid[row]) and grid[row][col] != " ")
        mxr = max(row for row in range(len(grid)) if col < len(grid[row]) and grid[row][col] != " ")
        jumps[(mnr - 1) + col * 1j] = mxr + col * 1j
        jumps[mxr + 1 + col * 1j] = mnr + col * 1j
    startc = min(col for col in range(len(grid[0])) if grid[0][col] != " ")
    mypos = startc * 1j
    mydir = 1j
    for (dist, turn) in moves:
        for _ in range(dist):
            nxt = jumps.get(mypos + mydir, mypos + mydir)
            if space[nxt] == "#":
                break
            mypos = nxt
        if turn == "R":
            mydir *= 0 - 1j
        elif turn == "L":
            mydir *= 0 + 1j
        else:
            assert False, "Bad turn"

    mydir *= 0 + 1j
    frow = int(mypos.real) + 1
    fcol = int(mypos.imag) + 1
    if mydir == 1j:
        fdir = 0
    elif mydir == -1.0:
        fdir = 1
    elif mydir == -1j:
        fdir = 2
    elif mydir == 1.0:
        fdir = 3
    # print(frow, fcol, fdir)
    print(1000 * frow + 4 * fcol + fdir)


@dataclass
class Face:
    # localZ cross localReal == localImag
    localz: Tuple[int, int, int]
    localreal: Tuple[int, int, int]
    localimag: Tuple[int, int, int]
    gridstart: complex
    spacestart: Tuple[int, int, int]


def get_unique(iterable, msg):
    l = list(iterable)
    if len(l) == 1:
        return l[0]
    assert False, f"length {len(l)}" + msg


def tuptimes(t_in, mul):
    return tuple(x * mul for x in t_in)


def tupplus(t_a, t_b):
    return tuple(a + b for (a, b) in zip(t_a, t_b))


def mint(z):
    assert z.imag == 0
    return int(z.real)


def part2(grid, moves):
    jumps = {}
    gridh = {}
    for row in range(len(grid)):
        for col in range(len(grid[row])):
            if grid[row][col] != " ":
                gridh[row + col * 1j] = grid[row][col]

    gridsize = int(math.sqrt(len(gridh) / 6))
    assert 6 * (gridsize**2) == len(gridh), f"gridsize calc error from {len(gridh)}"
    facestarts = list(
        filter(
            lambda x: x in gridh,
            [r * gridsize + c * gridsize * 1j for c in range(10) for r in range(10)],
        )
    )
    start = min(facestarts, key=lambda z: (z.real, z.imag))
    # faces
    faces: List[Face] = [Face((0, 0, 1), (1, 0, 0), (0, 1, 0), start, (0, 0, 0))]
    foundstarts = [start]
    while len(foundstarts) < 6:
        for face in faces:
            for seekdir in 1, -1:
                tststart = face.gridstart + gridsize * seekdir
                if tststart in facestarts and tststart not in foundstarts:
                    new_lz = tuptimes(face.localreal, seekdir)
                    new_lr = tuptimes(face.localz, -seekdir)
                    startadj = 0
                    if seekdir == 1:
                        startadj = tuptimes(face.localreal, gridsize - 1)
                    if seekdir == -1:
                        startadj = tuptimes(face.localz, -(gridsize - 1))
                    faces.append(
                        Face(
                            new_lz,
                            new_lr,
                            face.localimag,
                            tststart,
                            tupplus(face.spacestart, startadj),
                        )
                    )
                    foundstarts.append(tststart)
            for seekdir in 1j, -1j:
                tststart = face.gridstart + gridsize * seekdir
                if tststart in facestarts and tststart not in foundstarts:
                    new_lz = tuple(mint(-1j * seekdir * co) for co in face.localimag)
                    new_li = tuple(mint(1j * seekdir * co) for co in face.localz)
                    if seekdir == 1j:
                        startadj = tuptimes(face.localimag, gridsize - 1)
                    if seekdir == -1j:
                        startadj = tuptimes(face.localz, -(gridsize - 1))
                    faces.append(
                        Face(
                            new_lz,
                            face.localreal,
                            new_li,
                            tststart,
                            tupplus(face.spacestart, startadj),
                        )
                    )
                    foundstarts.append(tststart)
    assert len(faces) == 6
    assert len(set(f.localz for f in faces)) == 6, f"Some faces folded on top of each other; only {len(set(f.localz for f in faces))} faces total"
    # make jumps
    for (face, to_face) in itertools.combinations(faces, 2):
        if face.localz == tuptimes(to_face.localz, -1):
            continue

        if to_face.localz == tuptimes(face.localreal, -1):
            seekdir = -1.0
        elif to_face.localz == tuptimes(face.localreal, 1):
            seekdir = 1.0
        elif to_face.localz == tuptimes(face.localimag, -1):
            seekdir = -1j
        elif to_face.localz == tuptimes(face.localimag, 1):
            seekdir = 1j
        else:
            assert False, f"Bad faces: {face} to {to_face}"

        if face.localz == tuptimes(to_face.localreal, -1):
            rseekdir = -1.0
        elif face.localz == tuptimes(to_face.localreal, 1):
            rseekdir = 1.0
        elif face.localz == tuptimes(to_face.localimag, -1):
            rseekdir = -1j
        elif face.localz == tuptimes(to_face.localimag, 1):
            rseekdir = 1j
        else:
            assert False, f"Bad faces: {face} to {to_face}"

        space_from = {}
        space_to = {}

        def space_xform(f: Face, z):
            return tupplus(
                f.spacestart,
                tupplus(tuptimes(f.localreal, int(z.real)), tuptimes(f.localimag, z.imag)),
            )
        
        linestart = ((1+1j)*seekdir + 1+1j)*(gridsize - 1)/2
        linedir = seekdir*(-1j)
        for jump in range(gridsize):
            z = linestart + jump*linedir
            space_from[space_xform(face, z)] = face.gridstart + z
        linestart = ((1+1j)*rseekdir + 1+1j)*(gridsize - 1)/2
        linedir = rseekdir*(-1j)
        for jump in range(gridsize):
            z = linestart + jump*linedir
            space_to[space_xform(to_face, z)] = to_face.gridstart + z

        space_from2 = {}
        space_to2 = {}
        for v1 in range(gridsize):
            for v2 in (0, gridsize - 1):
                for (lrow, lcol) in [(v1, v2), (v2, v1)]:
                    z = lrow + lcol * 1j
                    space_from2[space_xform(face, z)] = face.gridstart + z
                    space_to2[space_xform(to_face, z)] = to_face.gridstart + z

        assert gridsize == len(set(space_from) & set(space_to))
        for space_pt in set(space_from) & set(space_to):
            von = space_from[space_pt]
            zu = space_to[space_pt]
            jumps[(von, seekdir)] = (zu, -rseekdir)
            jumps[(zu, rseekdir)] = (von, -seekdir)

    for ((startspot, startdir), (nxtspot, nxtdir)) in jumps.items():
        if (nxtspot, -nxtdir) not in jumps:
            assert (
                False
            ), f"Couldn't jump from {(startspot, startdir)} to {(nxtspot, nxtdir)} and back"
        if jumps[(nxtspot, -nxtdir)] != (startspot, -startdir):
            assert (
                False
            ), f"jumps[{(startspot, startdir)}] = {(nxtspot, nxtdir)} but jumps[{(nxtspot, -nxtdir)}] = {jumps[(nxtspot, -nxtdir)] }"
        assert (gridh.get(startspot + startdir, " ") == " ") != (
            nxtspot == startspot + startdir
        ), f"{((startspot, startdir), (nxtspot, nxtdir))}"

    # def take_jump(mypos, mydir):
    #     (nxt, nxtdir) = jumps.get((mypos,mydir), (mypos+mydir, mydir))
    #     if (nxt, nxtdir) != (mypos+mydir, mydir):
    #         print(f"Jumping from {(mypos,mydir)} to {(nxt, nxtdir)}")
    #     return (nxt, nxtdir)

    startc = min(col for col in range(len(grid[0])) if grid[0][col] != " ")
    mypos = startc * 1j
    mydir = 1j
    for (dist, turn) in moves:
        for _ in range(dist):
            (nxt, nxtdir) = jumps.get(
                (mypos, mydir), (mypos + mydir, mydir)
            )  # take_jump(mypos, mydir)
            assert gridh.get(nxt, " ") != " ", f"Walked wrong to {nxt} from {mypos} and {mydir}"
            if gridh[nxt] == "#":
                break
            # print(f"moving to {nxt}")
            mypos = nxt
            mydir = nxtdir
            grid[int(mypos.real)][int(mypos.imag)] = {
                1: "v",
                -1: "^",
                1j: ">",
                -1j: "<",
            }[mydir]
        if turn == "R":
            mydir *= 0 - 1j
        elif turn == "L":
            mydir *= 0 + 1j
        else:
            assert False, "Bad turn"
        grid[int(mypos.real)][int(mypos.imag)] = {1: "v", -1: "^", 1j: ">", -1j: "<"}[mydir]

    mydir *= 0 + 1j
    frow = int(mypos.real) + 1
    fcol = int(mypos.imag) + 1
    if mydir == 1j:
        fdir = 0
    elif mydir == 1.0:
        fdir = 1
    elif mydir == -1j:
        fdir = 2
    elif mydir == -1.0:
        fdir = 3
    # for line in grid:
    #     print(''.join(line))
    # print(frow, fcol, fdir)
    print(1000 * frow + 4 * fcol + fdir)


def main():
    data = get_data_paras(22)

    grid = chargrid(data[0])
    moves = [(int(s[:-1]), s[-1]) for s in re.findall(r"\d+[LR]", data[1].strip() + "R")]
    part1(grid, moves)
    part2(grid, moves)


if __name__ == "__main__":
    main()
