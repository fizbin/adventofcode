"""
Advent of code 2021 day 23 in python.
"""

import re
import heapq

cost = {"A": 1, "B": 10, "C": 100, "D": 1000, ".": 0}


def potential_moves(size, state, i1=1, i2=1):
    # returns a list of (newstate, cost)
    retval = []
    hall = state[0]
    for hall_loc in (0, 1, 3, 5, 7, 9, 10):
        if hall[hall_loc] == ".":
            for (idx, doorway) in enumerate((2, 4, 6, 8)):
                if state[idx + 1]:
                    amph = state[idx + 1][0]
                    tohallcost = cost[amph] * (1 + size - len(state[idx + 1]))
                    intermed = ""
                    if hall_loc < doorway:
                        intermed = hall[hall_loc:doorway]
                    else:
                        intermed = hall[doorway + 1 : hall_loc + 1]
                    if re.match(r"^[.]*$", intermed) is None:
                        continue
                    mutstate = list(state)
                    mutstate[idx + 1] = state[idx + 1][1:]
                    mcost = tohallcost + cost[amph] * len(intermed)
                    mutstate[0] = hall[0:hall_loc] + amph + hall[hall_loc + 1 :]
                    retval.append((tuple(mutstate), mcost))
        else:
            # to room?
            amph = hall[hall_loc : hall_loc + 1]
            dest_room = {"A": 1, "B": 2, "C": 3, "D": 4}[amph]
            doorway = {"A": 2, "B": 4, "C": 6, "D": 8}[amph]
            if len(state[dest_room]) >= size or state[dest_room] not in (
                "",
                amph,
                amph * 2,
                amph * 3,
            ):
                continue
            if hall_loc < doorway:
                intermed = hall[hall_loc + 1 : doorway + 1]
            else:
                intermed = hall[doorway:hall_loc]
            if re.match(r"^[.]*$", intermed) is None:
                continue
            mcost = cost[amph] * (size - len(state[dest_room]) + len(intermed))
            mutstate = list(state)
            mutstate[0] = hall[0:hall_loc] + "." + hall[hall_loc + 1 :]
            mutstate[dest_room] += amph
            retval.append((tuple(mutstate), mcost))
    return retval


def run_prog(size, initstate, endstate):
    seen = {}
    queue = [(0, initstate)]
    while queue:
        (mcost, mstate) = heapq.heappop(queue)
        if mstate == endstate:
            print(mcost)
            break
        for (newstate, newcost) in potential_moves(size, mstate):
            fcost = mcost + newcost
            if seen.get(newstate, fcost + 1) > fcost:
                heapq.heappush(queue, (fcost, newstate))
                seen[newstate] = fcost
        # print('cc', mcost, len(queue))


if __name__ == "__main__":
    # with open("aoc23.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
    #     data = re.findall(
    #         r"(on|off) x=([-\d]+)\.\.([-\d]+),y=([-\d]+)\.\.([-\d]+),z=([-\d]+)\.\.([-\d]+)",
    #         f.read(),
    #     )
    # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    # data = re.findall(r"(\w+)-(\w+)", f.read())
    # data = re.findall(r"(-?\d+)\.\.(-?\d+)", f.read())
    # data = re.findall(r"scanner [^s]*", f.read())
    # data = re.findall(r"\S+", f.read())
    initstate = ("...........", "DC", "BC", "DA", "AB")
    endstate = ("...........", "AA", "BB", "CC", "DD")
    run_prog(2, initstate, endstate)
    initstate = ("...........", "DDDC", "BCBC", "DBAA", "AACB")
    endstate = ("...........", "AAAA", "BBBB", "CCCC", "DDDD")
    run_prog(4, initstate, endstate)
