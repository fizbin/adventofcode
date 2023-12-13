#!/usr/bin/python
import aoc_util
import sys

data = aoc_util.get_data_paras(13)

total = 0
for idx, para in enumerate(data):
    grid = aoc_util.chargrid(para)
    for reflect_left in range(1, len(grid[0])):
        rsize = min(reflect_left, len(grid[0]) - reflect_left)
        found = all(
            line[:reflect_left][-rsize:] == list(reversed(line[reflect_left:][:rsize]))
            for line in grid
        )
        if found:
            total += reflect_left
            break
    else:
        for reflect_top in range(1, len(grid)):
            rsize = min(reflect_top, len(grid) - reflect_top)
            found = all(
                grid[idx1] == grid[idx2]
                for (idx1, idx2) in zip(
                    range(reflect_top - rsize, reflect_top),
                    reversed(range(reflect_top, reflect_top + rsize)),
                )
            )
            if found:
                total += reflect_top * 100
                break
        else:
            print("---------------ERR---------")
            print(f"NO REFLECTION FOUND for {idx}")
            print()
            print(para)
            print()
            sys.exit(1)

print(total)

total = 0

for idx, para in enumerate(data):
    grid = aoc_util.chargrid(para)
    for reflect_left in range(1, len(grid[0])):
        rsize = min(reflect_left, len(grid[0]) - reflect_left)
        imperfections = sum(
            lc != rc
            for line in grid
            for (lc, rc) in zip(
                line[:reflect_left][-rsize:], list(reversed(line[reflect_left:][:rsize]))
            )
        )
        if imperfections == 1:
            total += reflect_left
            break
    else:
        for reflect_top in range(1, len(grid)):
            rsize = min(reflect_top, len(grid) - reflect_top)
            imperfections = sum(
                lc != rc
                for (idx1, idx2) in zip(
                    range(reflect_top - rsize, reflect_top),
                    reversed(range(reflect_top, reflect_top + rsize)),
                )
                for (lc, rc) in zip(grid[idx1], grid[idx2])
            )
            if imperfections == 1:
                total += reflect_top * 100
                break
        else:
            print("---------------ERR---------")
            print(f"NO IMPERFECT REFLECTION FOUND for {idx}")
            print()
            print(para)
            print()
            sys.exit(1)

print(total)
