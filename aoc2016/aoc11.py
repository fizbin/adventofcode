import sys
import re
import heapq
import itertools


infile = "aoc11.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = [x.strip() for x in list(infilep)]

part1_floor_contents: list[list[tuple[int, str]]] = []
for line in data:
    (_ignore, rest) = line.split("contains")
    part1_floor_contents.append([])
    for m in re.finditer(r"(\w+) generator", rest):
        part1_floor_contents[-1].append((0, m.group(1)))
    for m in re.finditer(r"(\w+)-compatible microchip", rest):
        part1_floor_contents[-1].append((1, m.group(1)))


def is_valid_state(state):
    if any(x < 0 or x > 3 for x in state):
        return False
    rad_floors = set(state[0 : (len(state) // 2)])
    for chip_idx in range(len(state) // 2, len(state) - 1):
        if state[chip_idx] in rad_floors:
            if state[chip_idx - (len(state) // 2)] != state[chip_idx]:
                return False
    return True


def get_state_equiv_class(state: list[int]):
    tups = []
    for chip_idx in range(len(state) // 2, len(state) - 1):
        tups.append((state[chip_idx - (len(state) // 2)], state[chip_idx]))
    return tuple(sorted(tups)) + (state[-1],)


def do_problem(floor_contents):
    all_contents = sum(floor_contents, [])
    all_contents.sort()

    # print(f"dbg: all_contents: {all_contents}")
    # print("dbg:", len(all_contents))

    # state is a tuple of (all floors for generators), (all floors for chips), elevator_floor
    # i.e., state is a tuple of ints, with length 2*(number of minerals) + 1
    # remember that we're zero-indexed, so we want everything on floor "3" and elevator
    # starts on floor "0"
    initial_state = tuple(
        max(
            floor
            for floor in range(len(floor_contents))
            if cts in floor_contents[floor]
        )
        for cts in all_contents
    ) + (0,)

    # print(f"dbg: initial_state: {initial_state}")
    # print("dbg:", len(initial_state))

    workqueue = [(0, 0, initial_state)]
    been_there = set()
    while workqueue:
        _ignored_est, dist, state = heapq.heappop(workqueue)
        equiv_class = get_state_equiv_class(state)
        if equiv_class in been_there:
            continue
        been_there.add(equiv_class)
        if all(x == 3 for x in state):
            break
        motile_idx = [
            idx for idx in range(0, len(state) - 1) if state[-1] == state[idx]
        ]
        for moving_idx in itertools.chain(
            map(lambda x: (x,), motile_idx), itertools.permutations(motile_idx, 2)
        ):
            lstate = list(state)
            for elevator_dir in (-1, 1):
                lstate[-1] += elevator_dir
                if 0 <= state[-1] <= 3:
                    for idx in moving_idx:
                        lstate[idx] = lstate[-1]
                    if is_valid_state(lstate):
                        rdist_estimate = (
                            3 * (len(lstate) - 1) - sum(lstate[:-1]) + 1
                        ) // 2
                        heapq.heappush(
                            workqueue,
                            (dist + 1 + rdist_estimate, dist + 1, tuple(lstate)),
                        )
                lstate[-1] -= elevator_dir

    print(dist)


do_problem(part1_floor_contents)

part2_floor_contents = list(part1_floor_contents)
part2_floor_contents[0].extend([(0, "ele"), (0, "dili"), (1, "ele"), (1, "dili")])
do_problem(part2_floor_contents)
