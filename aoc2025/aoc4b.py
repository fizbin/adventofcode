import aoc_util
import numpy as np
import numpy.typing as npt
from scipy.ndimage import convolve


def part1(data: npt.NDArray[np.int_]) -> tuple[npt.NDArray[np.intp], ...]:
    kernel = np.array([[1, 1, 1], [1, 0, 1], [1, 1, 1]], dtype=int)
    ans = convolve(data, kernel, mode="constant", cval=0)
    return np.where(data & (ans < 4))


def part2(data: npt.NDArray[np.int_]) -> int:
    initial = data.sum()
    rm_ed = part1(data)
    while len(rm_ed[0]):
        data[rm_ed] = 0
        rm_ed = part1(data)
    return initial - data.sum()


if __name__ == "__main__":
    data = np.genfromtxt(aoc_util.get_data_lines(4), dtype=str, delimiter=1)
    mangled = (data == "@").astype(int)
    print(len(part1(mangled)[0]))
    print(part2(mangled))
