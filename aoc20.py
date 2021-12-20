"""
Advent of code 2021 day 20 in python.
"""

import sys
import re
#import pprint


def enhancetwice(fullimage, key):
    nxtimage = [["."] * len(row) for row in fullimage]
    for row in range(len(fullimage)):
        for col in range(len(fullimage[0])):
            if (
                row == 0
                or col == 0
                or row == len(fullimage) - 1
                or col == len(fullimage[0]) - 1
            ):
                nxtimage[row][col] = key[0:1]
            else:
                keyval = (
                    fullimage[row - 1][col - 1]
                    + fullimage[row - 1][col]
                    + fullimage[row - 1][col + 1]
                )
                keyval += (
                    fullimage[row][col - 1]
                    + fullimage[row][col]
                    + fullimage[row][col + 1]
                )
                keyval += (
                    fullimage[row + 1][col - 1]
                    + fullimage[row + 1][col]
                    + fullimage[row + 1][col + 1]
                )
                keyval = re.sub("[.]", "0", keyval)
                keyval = re.sub("[#]", "1", keyval)
                keyval = int(keyval, 2)
                nxtimage[row][col] = key[keyval : keyval + 1]
    fullimage = nxtimage
    nxtimage = [["."] * len(row) for row in fullimage]
    for row in range(len(fullimage)):
        for col in range(len(fullimage[0])):
            if (
                row == 0
                or col == 0
                or row == len(fullimage) - 1
                or col == len(fullimage[0]) - 1
            ):
                nxtimage[row][col] = key[0:1] if key[0:1] == "." else key[-1:]
            else:
                keyval = (
                    fullimage[row - 1][col - 1]
                    + fullimage[row - 1][col]
                    + fullimage[row - 1][col + 1]
                )
                keyval += (
                    fullimage[row][col - 1]
                    + fullimage[row][col]
                    + fullimage[row][col + 1]
                )
                keyval += (
                    fullimage[row + 1][col - 1]
                    + fullimage[row + 1][col]
                    + fullimage[row + 1][col + 1]
                )
                keyval = re.sub("[.]", "0", keyval)
                keyval = re.sub("[#]", "1", keyval)
                keyval = int(keyval, 2)
                nxtimage[row][col] = key[keyval : keyval + 1]
    fullimage = nxtimage
    # maybe embiggen?

    if '#' in ''.join(''.join(row) for row in fullimage[0:4]):
        fullimage = [['.'] * len(fullimage[0]) for _ in range(4)] + fullimage
    if '#' in ''.join(''.join(row) for row in fullimage[-4:]):
        fullimage = fullimage + [['.'] * len(fullimage[0]) for _ in range(4)]
    if '#' in ''.join(''.join(row[0:4]) for row in fullimage):
        fullimage = [['.'] * 4 + row for row in fullimage]
    if '#' in ''.join(''.join(row[-4:]) for row in fullimage):
        fullimage = [row + ['.'] * 4 for row in fullimage]
    return fullimage


if __name__ == "__main__":
    with open("aoc20.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
        # data = re.findall(r"(\w+)-(\w+)", f.read())
        # data = re.findall(r"(-?\d+)\.\.(-?\d+)", f.read())
        # data = re.findall(r"scanner [^s]*", f.read())
        data = re.findall(r"\S+", f.read())
        # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
        # data = [tuple(int(y) for y in tup) for tup in data]
        key = data[0]
        image = data[1:]
    fullimage = []
    for fullrowidx in range(-4, len(image) + 5):
        fullimage.append([])
        for fullcolidx in range(-4, len(image[0]) + 5):
            if 0 <= fullrowidx < len(image) and 0 <= fullcolidx < len(image[0]):
                fullimage[-1].append(image[fullrowidx][fullcolidx : fullcolidx + 1])
            else:
                fullimage[-1].append(".")
    #pprint.pprint([''.join(row) for row in fullimage])
    fullimage = enhancetwice(fullimage, key)
    #pprint.pprint([''.join(row) for row in fullimage])
    count = 0
    for row in fullimage:
        for spot in row:
            if spot == "#":
                count += 1
    print(count)
    for _ in range(24):
        fullimage = enhancetwice(fullimage, key)
        #pprint.pprint([''.join(row) for row in fullimage])
    count = 0
    for row in fullimage:
        for spot in row:
            if spot == "#":
                count += 1
    print(count)
