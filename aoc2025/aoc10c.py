from typing import Sequence
import aoc_util
import numpy as np
import math
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


def row_reduce(extended_mat):
    """
    This gets as close to "reduced row echelon form" as is possible for a matrix
    constrained to `dtype=int`. Specifically:
    - The matrix is in row echelon form
    - Each pivot element is the only nonzero entry in its column
    - Each pivot element is positive, and the entries in any non-zero row have a gcd of 1

    (If we were working over floats, we'd say "each pivot element is 1" for that last
    condition instead; that's the usual definition of "reduced row echelon form")

    The matrix is manipulated into this form only by these operations:
    - Swap two rows
    - Scale a row by a non-zero value
    - rowA := rowA - (scalar * rowB), for two rows rowA and rowB, scalar != 0

    One thing this means is that if you think of a matrix as representing the coefficients
    on several linear equations in the same variables, all of those operations don't change
    the solution space of the set of linear equations.

    :param extended_mat: Starting matrix, which is modified in-place and returned.
    """
    rowcount, colcount = extended_mat.shape
    working_row = 0
    for col_target in range(min(rowcount, colcount)):
        if extended_mat[working_row, col_target] == 0:
            # Uh-oh! Can we fix things by swapping rows with something below?
            for below in range(working_row + 1, rowcount):
                if extended_mat[below, col_target] != 0:
                    # Oh, good. Swap and continue
                    extended_mat[[working_row, below],] = extended_mat[[below, working_row],]
                    break
            else:
                # Well crud. I guess we skip this column
                continue
        if extended_mat[working_row, col_target] < 0:
            extended_mat[working_row] *= -1
        pivot_val = extended_mat[working_row, col_target]
        # now clear above & below
        for other_row in range(rowcount):
            if other_row == working_row:
                continue
            if extended_mat[other_row, col_target] != 0:
                lcm_target = math.lcm(pivot_val, extended_mat[other_row, col_target])
                extended_mat[other_row] *= abs(lcm_target // extended_mat[other_row, col_target])
                extended_mat[other_row] -= (
                    extended_mat[other_row, col_target] // pivot_val
                ) * extended_mat[working_row]
                row_gcd = math.gcd(*(extended_mat[other_row]))
                if row_gcd > 1:
                    extended_mat[other_row] //= row_gcd
        working_row += 1
    for working_row in range(rowcount):
        row_gcd = math.gcd(*(extended_mat[working_row]))
        if row_gcd > 1:
            extended_mat[working_row] //= row_gcd
    return extended_mat


def moves(buttons: list[Sequence[int]], jolts: Sequence[int]) -> int:
    jolt_col_vec = np.array([list(jolts)]).transpose().astype(int)
    button_mat = np.zeros((len(jolts), len(buttons)), dtype=int)
    for button_idx, button in enumerate(buttons):
        button_mat[tuple(button), button_idx] = 1

    # slap the jolts on to the right-hand-side of the button coefficient matrix
    combined = np.hstack((button_mat, jolt_col_vec))
    combined_reduced = row_reduce(combined)

    # Now we can have one of three cases:
    # - We have exactly enough information that there's a unique button solution
    #   (#buttons == #joltages, also all button combos linearly independent)
    #   In this case, `combined_reduced` looks like a diagonal matrix (square
    #   with non-0 stuff only on the diagonal) with one extra column of joltages
    #   slapped onto the right-hand-side.
    # - We have too much information (#joltages > #buttons), and have a unique
    #   solution and also some extra all-0 rows at the bottom of `combined_reduced`
    #   If we cut those extra rows off, `combined_reduced` looks like the first case
    # - We don't have enough information (#buttons > #joltages), so we'll have to
    #   add extra information to the system so that we get to where we can find
    #   a solution. We do this by imagining that there are some extra joltages
    #   that are each only connected to one of the buttons, and then try all the
    #   possible values those extra joltages could have and see what solution
    #   gives the smallest possible button sum.
    #   In this case, `combined_reduced` looks like a diagonal matrix with multiple
    #   columns slapped on the right-hand-side.
    #
    # There's probably a much more clever way to approach the third case than that
    # small bit of brute force, since I as a human can look at a `combined_reduced`
    # matrix in the third case, feed a slightly different matrix back through
    # `row_reduce` and from the result of that narrow down the cases that must be
    # checked to two or three instead of a few hundred but the computer is really
    # good at brute force, and I'm not so good at telling it how I'm doing the
    # math that I'm doing to prove I only need to check a few cases.

    # If we're in the third case, `test_maxes` is going to collect the maximum values
    # that we can set these new imagined joltages to.
    test_maxes: tuple[int, ...] = ()
    for button_idx, button in enumerate(buttons):
        if button_idx < combined_reduced.shape[0] and combined_reduced[button_idx, button_idx] > 0:
            continue

        # Okay, so we don't have an entry in the "diagonal" part of combined_reduced that
        # deals with this button. So we're going to add a new row to the bottom of
        # combined_reduced, and a new column to the right. This new row-and-column will
        # be all zero except that the bottom row will have a "1" in the column of button_idx
        # and a "1" in the last (new) column. We then run that (with the new row) through
        # 'row_reduce'
        combined_reduced = np.vstack(
            (
                np.hstack((combined_reduced, np.zeros((combined_reduced.shape[0], 1), dtype=int))),
                np.zeros((1, combined_reduced.shape[1] + 1), dtype=int),
            )
        )
        combined_reduced[-1, button_idx] = 1
        combined_reduced[-1, -1] = 1
        combined_reduced = row_reduce(combined_reduced)
        button_max = min(jolts[i] for i in button)
        test_maxes += (button_max,)

    # Now remove trailing all-0 rows (convert the second case above into the first case)
    while (combined_reduced[-1, :] == 0).all():
        combined_reduced = combined_reduced[0:-1, :]

    square_part, mul_part = np.hsplit(combined_reduced, [len(buttons)])

    # some paranoia assertions to keep us honest
    assert square_part.shape == (len(buttons), len(buttons))
    assert mul_part.shape == (len(buttons), 1 + len(test_maxes))

    # assert that square_part is a diagonal matrix with all positive entries on the diagonal
    assert ((square_part > 0) == (np.eye(len(buttons)) > 0)).all()
    assert ((square_part == 0) == (np.eye(len(buttons)) == 0)).all()

    necessary_mods = square_part.sum(axis=1)

    # An obviously much too large answer (stand in for "Infinity")
    ans = 1 << 63

    nib = np.zeros((1 + len(test_maxes),), dtype=int)
    nib[0] = 1
    while True:
        # At this point, "nib" is a column vector with a "1" up top and
        # then some extra joltage values we're filling in as guesses
        # so that we have enough information to find a button solution.
        m = mul_part @ nib
        # So now (m / necessary_mods) is our presumptive button solution, and
        # we check that it's valid
        if (m >= 0).all() and ((m % necessary_mods) == 0).all():
            ans = min(ans, (m // necessary_mods).sum())

        # now we increment the value in "nib"
        for chidx, maxval in enumerate(test_maxes):
            if nib[chidx + 1] + 1 > maxval:
                nib[chidx + 1] = 0
            else:
                nib[chidx + 1] += 1
                break  # out of the "for chidx, maxval" loop
        else:
            break  # out of the "while True" loop
    return ans


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
