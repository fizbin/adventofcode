import functools
from typing import Sequence
import aoc_util
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

    # A "pressing" is some combination of pressing buttons, each button pressed at most once
    # This is also what it means in part1, above.

    # list of (pressing_count, pressing_result)
    pressing_list: list[tuple[int, list[int]]] = []
    for pressing in range(2 ** len(buttons)):
        pcount = 0
        new_jolts = [0] * len(jolts)
        for b_idx, button in enumerate(buttons):
            if (2**b_idx) & pressing != 0:
                pcount += 1
                for idx in button:
                    new_jolts[idx] += 1
        pressing_list.append((pcount, new_jolts))
    pressing_map: dict[tuple[int, ...], list[tuple[int, list[int]]]] = {}
    for pcount, pjolts in pressing_list:
        sig = tuple(x % 2 for x in pjolts)
        pressing_map.setdefault(sig, []).append((pcount, pjolts))

    @functools.lru_cache
    def find_min(target_jolts: tuple[int]) -> int:
        if all(x == 0 for x in target_jolts):
            return 0
        minans = 1_000_000_000
        sig = tuple(x % 2 for x in target_jolts)
        for pcount, pjolts in pressing_map.get(sig, []):
            new_jolts = [x - y for (x, y) in zip(target_jolts, pjolts)]

            # This assert isn't needed, it's just to demonstrate that the map-by-sig does
            # what I want it to do
            assert all(x % 2 == 0 for x in new_jolts)

            if any(x < 0 for x in new_jolts):
                continue
            minans = min(minans, pcount + 2 * find_min(tuple(x // 2 for x in new_jolts)))
        return minans

    return find_min(tuple(jolts))


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
