from aoc_util import get_data_lines
import aoc_util

data = get_data_lines(18)

mymap = {}
for xidx in range(71):
    for yidx in range(71):
        mymap[(xidx, yidx)] = "."

for spotline in data[:1024]:
    (spotx, spoty) = spotline.split(",")
    mymap[(int(spotx), int(spoty))] = "#"


def get_neighbors(spot):
    (myx, myy) = spot
    nbs = [(myx - 1, myy), (myx, myy - 1), (myx, myy + 1), (myx + 1, myy)]
    return [s for s in nbs if mymap.get(s, "$") == "."]

print("Part 1: ", end='')
print(aoc_util.astar(start=(0, 0), goalf=lambda t: t == (70, 70), neighborf=get_neighbors)[0])


def check_fallamt(n):
    for xidx in range(71):
        for yidx in range(71):
            mymap[(xidx, yidx)] = "."

    for spotline in data[:n]:
        (spotx, spoty) = spotline.split(",")
        mymap[(int(spotx), int(spoty))] = "#"

    return (
        aoc_util.astar(start=(0, 0), goalf=lambda t: t == (70, 70), neighborf=get_neighbors)
        is not None
    )


lowval = 1023
hival = len(data)

while hival - lowval > 1:
    midval = (lowval + hival) // 2
    if check_fallamt(midval):
        lowval = midval
    else:
        hival = midval

print("Part 2: ", end='')
print(data[lowval])
