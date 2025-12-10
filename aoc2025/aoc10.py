from typing import Sequence
import aoc_util
import functools
import itertools
import heapq
import re


def part1(data: list[dict]) -> int:
    ans = 0
    for machine in data:
        target = machine['target']
        buttons = machine['buttons']
        minpcount = len(buttons) + 10
        for press in range(2**len(buttons)):
            val = 0
            pcount = 0
            for b_idx, button in enumerate(buttons):
                if (2**b_idx) & press != 0:
                    pcount += 1
                    val ^= button
            if val == target:
                minpcount = min(minpcount, pcount)
        ans += minpcount
    return ans


def moves(buttons: list[Sequence[int]], jolts: Sequence[int] ) -> int:
    try:
        jolts = tuple(jolts)
        heap = [(0, 0, tuple(0 for _ in jolts))]
        seen = set()
        while heap:
            (_, pressed, where) = heapq.heappop(heap)
            if where == jolts:
                return pressed
            for i in range(len(where)):
                if where[i] > jolts[i]:
                    continue
            # print(f"{(pressed, where)}")
            # if pressed > 5:
            #     raise ValueError(f"{where}")
            for button in buttons:
                new_where = tuple(where[i] + (1 if i in button else 0) for i in range(len(where)))
                if new_where in seen:
                    continue
                seen.add(new_where)
                min_remaining = max(jolts[i] - where[i] for i in range(len(where)))
                heapq.heappush(heap, (pressed+1+min_remaining, pressed+1, new_where))
        return None
    finally:
        print("DBG>", jolts)


def part2(data: list[dict]) -> int:
    ans = 0
    for machine in data:
        ans += moves(machine["jbuttons"], machine["jolts"])
    return ans


if __name__ == "__main__":
    datalines = aoc_util.get_data_lines(10)
    parser = re.compile(r"\[([.#]*)\] *((?:\([\d,]+\) *)+) *\{([\d,]+)\} *")
    machines = []
    for line in datalines:
        m = parser.fullmatch(line)
        assert m is not None
        pat = m.group(1)
        patnum = int(''.join(reversed(pat.replace('.', '0').replace("#", "1"))), 2)
        button_nums = []
        raw_buttons = []
        for button in m.group(2).replace("(","").split(")"):
            if button.strip():
                lnums = [int(x) for x in button.strip().split(",")]
                raw_buttons.append(tuple(lnums))
                button_nums.append(sum(2**x for x in lnums))
        joltages = [int(x) for x in m.group(3).split(",")]
        machines.append({"pat": pat,"target": patnum,"buttons": button_nums, "jbuttons": raw_buttons, "jolts": joltages})
    print(part1(machines))
    print(part2(machines))
