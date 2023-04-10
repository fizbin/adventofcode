from aoc_util import get_data_lines
import re
import heapq

data = get_data_lines(16)

valvemap = {}
valverates = {}

for line in data:
    m = re.match(
        r"Valve (\S+) has flow rate=(\d+); tunnels? leads? to valves? (.*)", line
    )
    if m is None:
        print("ERR: " + line)
    name = m.group(1)
    rate = int(m.group(2))
    rest = [dest.strip() for dest in m.group(3).split(",")]
    valvemap[name] = tuple(rest)
    valverates[name] = rate

# state is (released_so_far, loc, open_valves, time)

sum_of_valverates = sum(valverates.values())
all_spots = [(-(sum_of_valverates*30), 0, 'AA', (), ("AA",()), 0)]
best_so_far = None
been_here = {}
best_tup = None
while all_spots:
    (pot, released_so_far, loc, open_valves, pathtup, time) = heapq.heappop(all_spots)
    if been_here.get((loc, open_valves, time), -1) >= released_so_far:
        continue
    been_here[(loc, open_valves, time)] = released_so_far
    if best_so_far is not None and pot < best_so_far:
        continue
    if time == 30:
        if best_so_far is None:
            best_so_far = released_so_far
            best_tup = pathtup
        else:
            best_so_far = max(best_so_far, released_so_far)
            if best_so_far == released_so_far:
                best_tup = pathtup
        continue
    nreleased = sum(valverates[v] for v in open_valves) + released_so_far
    ntime = time + 1
    npot = nreleased + (30 - ntime) * sum_of_valverates
    for dest in valvemap[loc]:
        heapq.heappush(all_spots, (-npot, nreleased, dest, open_valves, (dest, pathtup), ntime))
    if valverates[loc] > 0 and loc not in open_valves:
        nopen_valves = tuple(sorted(open_valves + (loc,)))
        heapq.heappush(all_spots, (-npot, nreleased, loc, nopen_valves, (loc, pathtup), ntime))
    #print("diag: ", len(all_spots), time)

print(best_so_far)

realpath = []
mtup = best_tup
while len(mtup) == 2:
    realpath.append(mtup[0])
    mtup = mtup[1]
realpath.reverse()

good_valves = sorted((v for v in valverates if valverates[v] > 0), key=lambda v: -valverates[v])

all_spots = [(-(sum_of_valverates * 26), 0, "AA", "AA", (), 4)]
best_so_far = None
been_here = {}
while all_spots:
    (pot, released_so_far, locA, locB, open_valves, time) = heapq.heappop(all_spots)
    if been_here.get((locA, locB, open_valves, time), -1) >= released_so_far:
        continue
    been_here[(locA, locB, open_valves, time)] = released_so_far
    been_here[(locB, locA, open_valves, time)] = released_so_far
    if best_so_far is not None and pot < best_so_far:
        continue
    if time == 30:
        if best_so_far is None:
            best_so_far = released_so_far
        else:
            best_so_far = max(best_so_far, released_so_far)
        continue
    currrate = sum(valverates[v] for v in open_valves)
    nreleased = currrate + released_so_far
    ntime = time + 1
    npot = nreleased + (30 - ntime) * sum_of_valverates
    destAp = valvemap[locA]
    destBp = valvemap[locB]
    if valverates[locA] > 0 and locA not in open_valves:
        destAp = (locA,) + destAp
    if valverates[locB] > 0 and locB not in open_valves:
        destBp = (locB,) + destBp

    for destA in destAp:
        for destB in destBp:
            nopen_valves = open_valves
            if destA == locA:
                nopen_valves += (locA,)
            if destB == locB:
                nopen_valves += (locB,)
            nopen_valves = tuple(sorted(set(nopen_valves)))
            
            if been_here.get((destA, destB, nopen_valves, ntime), -1) < nreleased:
                heapq.heappush(
                    all_spots, (-npot, nreleased, destA, destB, nopen_valves, ntime)
                )
    #print("diag: ", len(all_spots), time, currrate, sum_of_valverates, "*" if currrate == sum_of_valverates else "")

print(best_so_far)
