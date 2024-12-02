from aoc_util import get_data_lines
import numpy as np

nprecs = [np.genfromtxt([x], dtype=int) for x in get_data_lines(2)]


def is_safe(x):
    chk = x[1:] - x[:-1]
    return ((chk > 0).all() or (chk < 0).all()) and ((abs(chk) >= 1) & (abs(chk) <= 3)).all()

def is_safe2(x):
    if is_safe(x):
        return True
    for r in range(x.size):
        if is_safe(np.delete(x, r)):
            return True
    return False

print("Part 1:", sum(1 for x in nprecs if is_safe(x)))
print("Part 2:", sum(1 for x in nprecs if is_safe2(x)))
