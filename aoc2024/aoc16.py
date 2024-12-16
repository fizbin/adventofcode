from aoc_util import get_data_lines
import aoc_util
import heapq

data = get_data_lines(16)

mymap = {}
start = None
endpos = None
for rowidx, row in enumerate(data):
    for colidx, ch in enumerate(row):
        mymap[rowidx + 1j * colidx] = ch
        if ch == "S":
            start = rowidx + 1j * colidx
        if ch == "E":
            endpos = rowidx + 1j * colidx


def get_neighbors(state: tuple[complex, complex]):
    (spot, mydir) = state
    retval = []
    if mymap.get(spot + mydir, "#") != "#":
        retval += [((spot + mydir, mydir), 1)]
    retval += [((spot, mydir * 1j), 1000), ((spot, -mydir * 1j), 1000)]
    return retval


(total, path) = aoc_util.astar(
    start=(start, 1j), goalf=lambda s: mymap.get(s[0]) == "E", neighbor_distf=get_neighbors
)
print("Part 1:", total)

mydist = {}  # state -> lowest dist

pushed = {}
workq = [(0, 0, "", (spot, ()), spot) for spot in [(start, 1j)]]
while workq:
    (_, cost_so_far, _s, path_tup, pos) = heapq.heappop(workq)
    if pos in mydist:
        continue
    if cost_so_far > total:
        break
    mydist[pos] = cost_so_far
    for next_spot, dist in get_neighbors(pos):
        if next_spot not in pushed or pushed[next_spot] > cost_so_far + dist:
            estimate = 0
            heapq.heappush(
                workq,
                (
                    cost_so_far + estimate + dist,
                    cost_so_far + dist,
                    str(next_spot),
                    (next_spot, path_tup),
                    next_spot,
                ),
            )
            pushed[next_spot] = cost_so_far + dist


def get_neighbors_backwards(state: tuple[complex, complex]):
    (spot, mydir) = state
    retval = []
    if mymap.get(spot - mydir, "#") != "#":
        retval += [((spot - mydir, mydir), 1)]
    retval += [((spot, mydir * 1j), 1000), ((spot, -mydir * 1j), 1000)]
    return retval


goodset = set([endpos])

visited = set()
working = [x for x in mydist if mydist[x] == total and x[0] == endpos]
while working:
    oworking = list(working)
    working = []
    for state in oworking:
        if state in visited:
            continue
        visited.add(state)
        goodset.add(state[0])
        nbrs = get_neighbors_backwards(state)
        if not nbrs:
            continue
        mycost = mydist[state]
        for nbr, cost in nbrs:
            if mydist.get(nbr, -1) == mycost - cost:
                working.append(nbr)


print("Part 2:", len(goodset))

# lastwhere = 0+0j
# for where, ch in mymap.items():
#     if where.real != lastwhere.real:
#         print()
#     if where in goodset:
#         print('O', end='')
#     else:
#         print(ch, end='')
#     lastwhere = where
# print()
