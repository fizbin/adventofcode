from aoc_util import get_data_lines

data = get_data_lines(6)
cmap = {}
guardstart = None
for idx, row in enumerate(data):
    for cidx, spot in enumerate(row):
        cmap[idx + 1j * cidx] = spot
        if spot == "^":
            guardstart = idx + 1j * cidx
            cmap[idx + 1j * cidx] = "."

# My convention, established above when I read the input in,
# is that "up" is -1 and "right" is 0+1j, so to turn "up"
# into "right", I would need to multiply by:
turn_right = -1j


def check_obstacle(objstacle_spot):
    "Subroutine for part 2"
    statehist = set()
    guardspot = guardstart
    guarddir = -1
    if cmap[objstacle_spot] != ".":
        return False
    try:
        cmap[objstacle_spot] = "#"
        while guardspot in cmap:
            if (guardspot, guarddir) in statehist:
                return True
            statehist.add((guardspot, guarddir))
            while cmap.get(guardspot + guarddir) == "#":
                guarddir *= turn_right
            guardspot += guarddir
        return False
    finally:
        cmap[objstacle_spot] = "."


# part 1

guarddir = -1
guardspot = guardstart
guardhist = {guardspot}
while guardspot in cmap:
    guardhist.add(guardspot)
    while cmap.get(guardspot + guarddir) == "#":
        guarddir *= turn_right
    guardspot += guarddir

print("Part 1:", len(guardhist))

# part 2

total = 0
for obspot in guardhist:
    if check_obstacle(obspot):
        total += 1
print("Part 2:", total)
