import re
import copy

data = open('aoc18.in.txt').read()

ground = [list(ln.strip()) for ln in data.splitlines()]


def get(y, x):
    if y < 0 or x < 0:
        return ' '
    try:
        return ground[y][x]
    except IndexError:
        return ' '


snapshots = []
for g in range(1, 1000):
    ground2 = copy.deepcopy(ground)
    for (y, row) in enumerate(ground):
        for (x, val) in enumerate(row):
            neighbors = ''.join(get(y+a, x+b) for (a, b) in
                                [(-1, -1), (-1, 0), (-1, 1), (0, -1),
                                 (0, 1), (1, -1), (1, 0), (1, 1)])
            if val == '.':
                if re.search('[|].*[|].*[|]', neighbors):
                    ground2[y][x] = '|'
            elif val == '|':
                if re.search('[#].*[#].*[#]', neighbors):
                    ground2[y][x] = '#'
            elif val == '#':
                if not re.search('[#].*[|]|[|].*[#]', neighbors):
                    ground2[y][x] = '.'
    ground = ground2

    snapshot = '\n'.join(''.join(row) for row in ground)
    if snapshot in snapshots:
        i = snapshots.index(snapshot)
        print("Found %d as a repeat of %d" % (g, 1+i))
        period = g - (1+i)
        while (i+1) % period != 1000000000 % period:
            i += 1
        # print(snapshots[i])
        count1 = len(re.findall('[|]', snapshots[i]))
        count2 = len(re.findall('[#]', snapshots[i]))
        print((i+1, count1, count2))
        print(count1 * count2)
        break
    snapshots.append(snapshot)

    if g == 10:
        count1 = len(re.findall('[|]', snapshot))
        count2 = len(re.findall('[#]', snapshot))
        print((g, count1, count2))
        print(count1 * count2)
