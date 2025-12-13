from typing import Sequence
import aoc_util
import heapq
import re


def part1(data: list[dict]) -> int:
    ans = 0
    for machine in data:
        target = machine["target"]
        buttons = machine["buttons"]
        minpcount = len(buttons) + 10
        for pressing in range(2 ** len(buttons)):
            val = 0
            pcount = 0
            for b_idx, button in enumerate(buttons):
                if (2**b_idx) & pressing != 0:
                    pcount += 1
                    val ^= button
            if val == target:
                minpcount = min(minpcount, pcount)
        ans += minpcount
    return ans


def moves(buttons: list[Sequence[int]], jolts: Sequence[int]) -> int:

    # I owe the initial insight that led me to this solution to the reddit post
    # https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/

    # So this is basically going to be just straight-up Dijkstra, but the moves and the
    # space we're walking in are a bit weird.

    # The space we're walking in is a tuple of (r, jolts_left), where jolts_left starts
    # at the tuple that we're trying to get to and goes down to (0, 0, 0, ..., 0)
    # (Yeah, I'm walking backwards; there's a good reason), and "r" is a power of 2;
    # r = 1, 2, 4, ... . I'm going to say that a value r "divides" a joltage list if
    # every number in the joltage list is a multiple of r. The only valid spots for
    # us to go to are (r, jolts_left) pairs were r divides jolts_left.

    # We start at (1, jolts) and run Dijkstra to get to (anything, (0, 0, ..., 0))

    # I'm calling a "pressing" a combination of pressing some of the buttons, each button
    # pressed at most once, and I call the number of buttons pressed in a pressing the
    # pressing's "pcount". So in part 1 of day 10, we were essentially finding which
    # pressings produced the correct light sequence and choosing the one with the smallest
    # pcount.

    # The way the graph is connected is this: (r, jolts1) is connected to (r*2, jolts2) iff:
    #   (r*2, jolts2) is a valid place (that is, r*2 divides jolts2), AND
    #   there's a pressing that when applied 'r' times would take you from jolts1 down to jolts2
    #   (remember that I'm going backwards)
    # If (r, jolts1) is connected to (r*2, jolts2) via a pressing "p", then the length of that
    # graph edge is r * (pcount of p)

    # Now, you can optimize slightly and not need to check all (2 ** len(buttons)) pressings
    # every time by precomputing which pressings will be able to jump from the (r, _) world
    # to the (r*2, _) world; that's what I'm doing below with press_by_sig, but I'll leave
    # that for you to figure out by reading the code.

    pressings = []
    press_by_sig = {}
    for press in range(2 ** len(buttons)):
        pressed_jolts = [0] * len(jolts)
        pcount = 0
        for b_idx, button in enumerate(buttons):
            if (2**b_idx) & press != 0:
                pcount += 1
                for j in button:
                    pressed_jolts[j] += 1
        press_by_sig.setdefault(tuple(j % 2 for j in pressed_jolts), []).append(press)
        pressings.append((pcount, tuple(pressed_jolts)))

    def allowed_moves(r_factor, jolts_left: Sequence[int]):
        sig = tuple((j // r_factor) % 2 for j in jolts_left)
        for press in press_by_sig.get(sig, []):
            (pcount, pressed_jolts) = pressings[press]
            new_jolts = [j_l - p_j * r_factor for (j_l, p_j) in zip(jolts_left, pressed_jolts)]
            if all(nj >= 0 for nj in new_jolts):
                yield (r_factor * pcount, tuple(new_jolts))

    heap = [(0, 1, tuple(jolts))]
    seen = set()
    while heap:
        (sofar, r, where) = heapq.heappop(heap)
        if (r, where) in seen:
            continue
        seen.add((r, where))
        # print(">", where, " */* ", end="")
        if all(j == 0 for j in where):
            return sofar
        for dist, where2 in allowed_moves(r, where):
            # print(">",where,"->->-",where2)
            heapq.heappush(heap, (sofar + dist, r * 2, where2))
    raise ValueError("Uh-oh")


def part2(data: list[dict]) -> int:
    ans = 0
    for idx, machine in enumerate(data):
        # print(f"DBG machine {idx}...")
        m = moves(machine["jbuttons"], machine["jolts"])
        # print(f"DBG machine {idx} -> {m}")
        ans += m
    return ans


if __name__ == "__main__":
    datalines = aoc_util.get_data_lines(10)
    parser = re.compile(r"\[([.#]*)\] *((?:\([\d,]+\) *)+) *\{([\d,]+)\} *")
    machines = []
    for line in datalines:
        m = parser.fullmatch(line)
        assert m is not None
        pat = m.group(1)
        patnum = int("".join(reversed(pat.replace(".", "0").replace("#", "1"))), 2)
        button_nums = []
        raw_buttons = []
        for button in m.group(2).replace("(", "").split(")"):
            if button.strip():
                lnums = [int(x) for x in button.strip().split(",")]
                raw_buttons.append(tuple(lnums))
                button_nums.append(sum(2**x for x in lnums))
        joltages = [int(x) for x in m.group(3).split(",")]
        machines.append(
            {
                "pat": pat,
                "target": patnum,
                "buttons": button_nums,
                "jbuttons": raw_buttons,
                "jolts": joltages,
            }
        )
    print(part1(machines))
    print(part2(machines))
