from typing import Sequence
import aoc_util
import itertools
import numpy as np
import re


def part1(data: list[dict]) -> int:
    ans = 0
    for machine in data:
        target = machine["target"]
        buttons = machine["buttons"]
        minpcount = len(buttons) + 10
        for press in range(2 ** len(buttons)):
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


def check_acceptable(x, button_mat, jolt_col_vec) -> int | None:
    for v in np.ravel(x):
        if abs(v - int(v + 0.01)) > 0.02:
            return None
        if int(v + 0.01) < 0:
            return None
    if np.abs(button_mat @ x - jolt_col_vec).sum() > 0.4:
        return None
    return int(x.sum() + 0.1)


def solve1(button_mat, jolt_col_vec) -> int | None:
    lenjolts, lenbuttons = button_mat.shape
    for row_combo in itertools.combinations(range(lenjolts), lenbuttons):
        reduced_bmat = button_mat[row_combo, :]
        reduced_jolts = jolt_col_vec[row_combo, :]
        if np.linalg.matrix_rank(reduced_bmat) == lenbuttons:
            x = np.linalg.solve(reduced_bmat, reduced_jolts)
            xtot = check_acceptable(x, button_mat, jolt_col_vec)
            return xtot
    return None


def extra_joltage_rows(buttons: list[Sequence[int]], jolts: Sequence[int]):
    for extra_joltage_reach in range(1, len(buttons) + 1):
        for jolt_sig in itertools.combinations(range(len(buttons)), extra_joltage_reach):
            max_val = 0
            for button_idx in jolt_sig:
                button = buttons[button_idx]
                button_max = min(jolts[b] for b in button)
                max_val += button_max
            yield (jolt_sig, max_val)


def moves(buttons: list[Sequence[int]], jolts: Sequence[int]) -> int:
    jolt_col_vec = np.array([list(jolts)]).transpose().astype(float)
    button_mat = np.zeros((len(jolts), len(buttons)), dtype=float)
    for button_idx, button in enumerate(buttons):
        button_mat[tuple(button), button_idx] = 1.0
    if np.linalg.matrix_rank(button_mat) >= len(buttons):
        # okay, we must have a unique solution, now we need to find
        # the right number of rows
        # This is roughly half the cases
        xtot = solve1(button_mat, jolt_col_vec)
        assert xtot is not None
        return xtot
    # okay, so rank is too small, so we try adding a few more constraints
    # until we get rank to be big enough
    bigger_button_mat = np.copy(button_mat)
    extra_jolts_max_vals = []
    while np.linalg.matrix_rank(bigger_button_mat) < len(buttons):
        t_bigger_button_mat = np.zeros((bigger_button_mat.shape[0] + 1, bigger_button_mat.shape[1]))
        t_bigger_button_mat[0:-1, :] = bigger_button_mat
        # t_bigger_button_mat[-1,:] = 1
        # if np.linalg.matrix_rank(bigger_button_mat) == np.linalg.matrix_rank(t_bigger_button_mat):
        #     print("Maybe shortcut?")
        for jolt_sig, jolt_max in extra_joltage_rows(buttons, jolts):
            t_bigger_button_mat[-1, :] = 0
            t_bigger_button_mat[-1, jolt_sig] = 1
            if np.linalg.matrix_rank(bigger_button_mat) < np.linalg.matrix_rank(
                t_bigger_button_mat
            ):
                bigger_button_mat = t_bigger_button_mat
                extra_jolts_max_vals.append(jolt_max)
                break
    # n_added = bigger_button_mat.shape[0] - button_mat.shape[0]
    # print(f"added {n_added}")
    # print(bigger_button_mat[-n_added:, :])
    min_presses = sum(jolts) + 1
    for extra_vals in itertools.product(*(range(v + 1) for v in extra_jolts_max_vals)):
        bigger_jolt_vec = np.array(list(jolts) + list(extra_vals)).reshape(
            (bigger_button_mat.shape[0], 1)
        )
        xtot = solve1(bigger_button_mat, bigger_jolt_vec)
        if xtot is not None and xtot < min_presses:
            # t_e = extra_vals
            min_presses = min(xtot, min_presses)
    if min_presses > sum(jolts):
        raise ValueError("Crap")
    # print(f"Used {t_e} (out of {extra_jolts_max_vals})")
    return min_presses


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
