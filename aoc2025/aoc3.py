import aoc_util
import itertools
import functools


def part1(data):
    ans = []
    for line in data:
        x = 0
        for val1, val2 in itertools.combinations(line.strip(), 2):
            x = max(x, int(val1 + val2))
        ans.append(x)
    return sum(ans)


@functools.lru_cache()
def largest_jolt(line, n):
    if not line or n == 0:
        return -1
    if n == 1:
        return int(max(line))
    ans = -1
    maxstart = max(line[: len(line) - n + 1])
    for idx in range(len(line) - n + 1):
        part = line[idx : idx + 1]
        if part < maxstart:
            continue
        rest = line[idx + 1 :]
        restans = largest_jolt(rest, n - 1)
        if restans < 0:
            continue
        candidate = int(part + str(restans))
        ans = max(ans, candidate)
    return ans


def part2(data):
    ans = []
    for line in data:
        ans.append(largest_jolt(line.strip(), 12))
    # print("> ", ans)
    return sum(ans)


if __name__ == "__main__":
    data = aoc_util.get_data_lines(3)
    print(part1(data))
    print(part2(data))
