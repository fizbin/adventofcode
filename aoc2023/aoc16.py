#!/usr/bin/python

import aoc_util

data = aoc_util.get_data(16)
grid = aoc_util.chargrid(data)
height = len(grid)
width = len(grid[0])


def find_lighted(initspot, initdir):
    beams = [(initspot, initdir)]
    handled = set()
    lighted = set()
    while beams:
        newbeams = []
        for beam in beams:
            if (
                beam in handled
                or not (0 <= beam[0].real < height)
                or not (0 <= beam[0].imag < width)
            ):
                continue
            handled.add(beam)
            lighted.add(beam[0])
            ch = grid[int(beam[0].real)][int(beam[0].imag)]
            if ch == "/":
                ndir = -1j / beam[1]
                newbeams.append((beam[0] + ndir, ndir))
                continue
            if ch == "\\":
                ndir = 1j / beam[1]
                newbeams.append((beam[0] + ndir, ndir))
                continue
            if ch == "|" and beam[1].real == 0:
                newbeams.append((beam[0] - 1, -1))
                newbeams.append((beam[0] + 1, 1))
                continue
            if ch == "-" and beam[1].imag == 0:
                newbeams.append((beam[0] - 1j, -1j))
                newbeams.append((beam[0] + 1j, 1j))
                continue
            newbeams.append((beam[0] + beam[1], beam[1]))
        beams = newbeams
    return len(lighted)


print(find_lighted(0, 1j))
print()
print(
    max(
        max(find_lighted(x, 1j) for x in range(height)),
        max(find_lighted(x + (width - 1) * 1j, -1j) for x in range(height)),
        max(find_lighted(y * 1j, 1) for y in range(width)),
        max(find_lighted(height - 1 + y * 1j, -1) for y in range(width)),
    )
)
