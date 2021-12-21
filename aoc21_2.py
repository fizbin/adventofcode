"""
Advent of code 2021 day 21, part two in python.
"""

import sys
import re
import collections
import functools
import itertools


if __name__ == "__main__":
    with open("aoc21.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
        # data = re.findall(r"(\w+)-(\w+)", f.read())
        # data = re.findall(r"(-?\d+)\.\.(-?\d+)", f.read())
        # data = re.findall(r"scanner [^s]*", f.read())
        data = re.findall(r"\d+", f.read())
        # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
        # data = [tuple(int(y) for y in tup) for tup in data]
        start1 = int(data[1])
        start2 = int(data[3])
    uni_totals = {(start1, start2, 0, 0, 1): 1}
    keep_going = True
    ooo = 0
    while keep_going:
        keep_going = False
        new_universe = collections.Counter()
        for ((spot1, spot2, score1, score2, turn), nuniverses) in uni_totals.items():
            if (score1 < 21) and (score2 < 21):
                if turn == 1:
                    new_universe[
                        (
                            (spot1 + 2) % 10 + 1,
                            spot2,
                            score1 + (spot1 + 2) % 10 + 1,
                            score2,
                            2,
                        )
                    ] += (1 * nuniverses)
                    new_universe[
                        (
                            (spot1 + 3) % 10 + 1,
                            spot2,
                            score1 + (spot1 + 3) % 10 + 1,
                            score2,
                            2,
                        )
                    ] += (3 * nuniverses)
                    new_universe[
                        (
                            (spot1 + 4) % 10 + 1,
                            spot2,
                            score1 + (spot1 + 4) % 10 + 1,
                            score2,
                            2,
                        )
                    ] += (6 * nuniverses)
                    new_universe[
                        (
                            (spot1 + 5) % 10 + 1,
                            spot2,
                            score1 + (spot1 + 5) % 10 + 1,
                            score2,
                            2,
                        )
                    ] += (7 * nuniverses)
                    new_universe[
                        (
                            (spot1 + 6) % 10 + 1,
                            spot2,
                            score1 + (spot1 + 6) % 10 + 1,
                            score2,
                            2,
                        )
                    ] += (6 * nuniverses)
                    new_universe[
                        (
                            (spot1 + 7) % 10 + 1,
                            spot2,
                            score1 + (spot1 + 7) % 10 + 1,
                            score2,
                            2,
                        )
                    ] += (3 * nuniverses)
                    new_universe[
                        (
                            (spot1 + 8) % 10 + 1,
                            spot2,
                            score1 + (spot1 + 8) % 10 + 1,
                            score2,
                            2,
                        )
                    ] += (1 * nuniverses)
                else:
                    new_universe[
                        (
                            spot1,
                            (spot2 + 2) % 10 + 1,
                            score1,
                            score2 + (spot2 + 2) % 10 + 1,
                            1,
                        )
                    ] += (1 * nuniverses)
                    new_universe[
                        (
                            spot1,
                            (spot2 + 3) % 10 + 1,
                            score1,
                            score2 + (spot2 + 3) % 10 + 1,
                            1,
                        )
                    ] += (3 * nuniverses)
                    new_universe[
                        (
                            spot1,
                            (spot2 + 4) % 10 + 1,
                            score1,
                            score2 + (spot2 + 4) % 10 + 1,
                            1,
                        )
                    ] += (6 * nuniverses)
                    new_universe[
                        (
                            spot1,
                            (spot2 + 5) % 10 + 1,
                            score1,
                            score2 + (spot2 + 5) % 10 + 1,
                            1,
                        )
                    ] += (7 * nuniverses)
                    new_universe[
                        (
                            spot1,
                            (spot2 + 6) % 10 + 1,
                            score1,
                            score2 + (spot2 + 6) % 10 + 1,
                            1,
                        )
                    ] += (6 * nuniverses)
                    new_universe[
                        (
                            spot1,
                            (spot2 + 7) % 10 + 1,
                            score1,
                            score2 + (spot2 + 7) % 10 + 1,
                            1,
                        )
                    ] += (3 * nuniverses)
                    new_universe[
                        (
                            spot1,
                            (spot2 + 8) % 10 + 1,
                            score1,
                            score2 + (spot2 + 8) % 10 + 1,
                            1,
                        )
                    ] += (1 * nuniverses)
                keep_going = True
            else:
                new_universe[(spot1, spot2, score1, score2, turn)] += nuniverses
        uni_totals = new_universe
    p1_wins = 0
    p2_wins = 0
    for ((spot1, spot2, score1, score2, turn), nuniverses) in uni_totals.items():
        if score1 > score2:
            p1_wins += nuniverses
        else:
            p2_wins += nuniverses
    print(max(p1_wins, p2_wins))
