from aoc_util import *
import re
import itertools
import heapq
import functools


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
    elif mydir == 1.0:
        fdir = 1
    elif mydir == -1j:
        fdir = 2
    elif mydir == -1.0:
        fdir = 3
    # print(frow, fcol, fdir)
    print(1000 * frow + 4 * fcol + fdir)


def part2(grid, moves):
    jumps = {}
    space = {}
    for row in range(len(grid)):
        for col in range(len(grid[row])):
            space[row + col * 1j] = grid[row][col]

    # My input data arrangement:
    #  56
    #  4
    # 23
    # 1
    for row in range(150, 200):
        col = 0
        jumps[(row + col * 1j, -1j)] = (0 + (50 + row - 150) * 1j, 1.0)

        col = 49
        jumps[(row + col * 1j, 1j)] = (149 + (50 + row - 150) * 1j, -1.0)

    for row in range(100, 150):
        col = 0
        jumps[(row + col * 1j, -1j)] = (0 + (149 - row) + 50j, 1j)

        col = 99
        jumps[(row + col * 1j, 1j)] = ((149 - row) + 149j, -1j)

    for row in range(50, 100):
        col = 50
        jumps[(row + col * 1j, -1j)] = (100 + (row - 50) * 1j, 1.0)

        col = 99
        jumps[(row + col * 1j, 1j)] = (49 + (100 + row - 50) * 1j, -1.0)

    for row in range(0, 50):
        col = 50
        jumps[(row + col * 1j, -1j)] = (100 + 49 - row + 0j, 1j)

        col = 149
        jumps[(row + col * 1j, 1j)] = (100 + (49 - row) + 99j, -1j)

    for col in range(100, 150):
        row = 0
        jumps[(row + col * 1j, -1.0)] = (199 + (col - 100) * 1j, -1.0)

        row = 49
        jumps[(row + col * 1j, 1.0)] = (50 + col - 100 + 99j, -1j)

    for col in range(0, 50):
        row = 100
        jumps[(row + col * 1j, -1.0)] = (50 + col + 50j, 1j)

        row = 199
        jumps[(row + col * 1j, 1.0)] = (0 + (100 + col) * 1j, 1.0)

    for col in range(50, 100):
        row = 0
        jumps[(row + col * 1j, -1.0)] = (150 + col - 50 + 0j, 1j)

        row = 149
        jumps[(row + col * 1j, 1.0)] = (150 + col - 50 + 49j, -1j)

    for ((startspot, startdir), (nxtspot, nxtdir)) in jumps.items():
        if (nxtspot, -nxtdir) not in jumps:
            assert (
                False
            ), f"Couldn't jump from {(startspot, startdir)} to {(nxtspot, nxtdir)} and back"
        if jumps[(nxtspot, -nxtdir)] != (startspot, -startdir):
            assert (
                False
            ), f"jumps[{(startspot, startdir)}] = {(nxtspot, nxtdir)} but jumps[{(nxtspot, -nxtdir)}] = {jumps[(nxtspot, -nxtdir)] }"
        assert (
            space.get(startspot + startdir, " ") == " "
        ), f"Unnecessary jump at {(startspot, startdir)}"

    startc = min(col for col in range(len(grid[0])) if grid[0][col] != " ")
    mypos = startc * 1j
    mydir = 1j
    for (dist, turn) in moves:
        for _ in range(dist):
            (nxt, nxtdir) = jumps.get((mypos, mydir), (mypos + mydir, mydir))
            assert space.get(nxt, " ") != " ", f"Walked wrong to {nxt} from {mypos} and {mydir}"
            if space[nxt] == "#":
                break
            mypos = nxt
            mydir = nxtdir
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
    elif mydir == 1.0:
        fdir = 1
    elif mydir == -1j:
        fdir = 2
    elif mydir == -1.0:
        fdir = 3
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
