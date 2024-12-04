from aoc_util import get_data_lines

grid = get_data_lines(4)
total = 0
direction = [(1, 1), (1, -1), (-1, -1), (-1, 1), (1, 0), (0, 1), (-1, 0), (0, -1)]
total = 0


def spot(x, y):
    if x < 0 or y < 0:
        return " "
    if x >= len(grid) or y >= len(grid[0]):
        return " "
    return grid[x][y]


for startx in range(len(grid)):
    for starty in range(len(grid[0])):
        if grid[startx][starty] == "X":
            for mydir in direction:
                if (
                    spot(startx + mydir[0], starty + mydir[1]) == "M"
                    and spot(startx + 2 * mydir[0], starty + 2 * mydir[1]) == "A"
                    and spot(startx + 3 * mydir[0], starty + 3 * mydir[1]) == "S"
                ):
                    total += 1
print("Part 1:", total)

total = 0
for startx in range(len(grid)):
    for starty in range(len(grid[0])):
        if grid[startx][starty] == "A":
            if (spot(startx + 1, starty + 1) == "M" and spot(startx - 1, starty - 1) == "S") or (
                spot(startx + 1, starty + 1) == "S" and spot(startx - 1, starty - 1) == "M"
            ):
                if (
                    spot(startx - 1, starty + 1) == "M" and spot(startx + 1, starty - 1) == "S"
                ) or (spot(startx - 1, starty + 1) == "S" and spot(startx + 1, starty - 1) == "M"):
                    total += 1
print("Part 2:", total)
