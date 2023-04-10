from aoc_util import *

data = chargrid(get_data(24))

pitchwidth = len(data[0]) - 2
pitchheight = len(data) - 2

for col, ch in enumerate(data[0]):
    if ch == ".":
        start = (0, col)

for col, ch in enumerate(data[len(data) - 1]):
    if ch == ".":
        goal = (len(data) - 1, col)

grid = {}
for row, line in enumerate(data):
    for col, ch in enumerate(line):
        grid[(row, col)] = ch


def neigh(spot):
    ((r, c), t) = spot
    basic = [(r, c), (r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]
    retval = []
    # breakpoint()
    for spotp in basic:
        if grid.get(spotp, "#") == "#":
            continue
        if 1 <= spotp[0] <= pitchheight:
            if grid[(spotp[0], 1 + (spotp[1] + t) % pitchwidth)] == "<":
                continue
            if grid[(spotp[0], 1 + (spotp[1] - t - 2) % pitchwidth)] == ">":
                continue
            if grid[(1 + (spotp[0] + t) % pitchheight, spotp[1])] == "^":
                continue
            if grid[(1 + (spotp[0] - t - 2) % pitchheight, spotp[1])] == "v":
                continue
        retval.append(spotp)
    retval2 = [(s, t + 1) for s in retval]
    # print(spot, ' -> ', r)
    # assert t < 30
    return retval2


def estimate(spot):
    return abs(goal[0] - spot[0][0]) + abs(spot[0][1] - goal[1])


# part 1
(time1, path1) = astar(
    start=(start, 0), goalf=lambda s: s[0] == goal, neighborf=neigh, estimatef=estimate
)
print(time1)

# part 2
(start, goal) = (goal, start)
(time2, path2) = astar(
    start=path1[-1], goalf=lambda s: s[0] == goal, neighborf=neigh, estimatef=estimate
)
(start, goal) = (goal, start)
(time3, path3) = astar(
    start=path2[-1], goalf=lambda s: s[0] == goal, neighborf=neigh, estimatef=estimate
)
print(path3[-1][1])
