import aoc_util
import re
import functools
import operator


def part1(data):
    ans = []
    for line in data:
        for idx, x in enumerate(re.findall(r"\S+", line)):
            if len(ans) <= idx:
                ans.append([int(x)])
            else:
                if re.fullmatch(r"\d+", x):
                    ans[idx].append(int(x))
                elif x == "*":
                    ans[idx] = functools.reduce(operator.mul, ans[idx], 1)
                elif x == "+":
                    ans[idx] = sum(ans[idx])
    return sum(ans)


def part2(data):
    transposed = [
        "".join(data[x][y] for x in range(len(data))) for y in reversed(range(len(data[0])))
    ]
    ans = []
    current = []
    for line in transposed:
        if not line.strip():
            continue
        if re.fullmatch(r"\s*\d+\s*", line):
            current.append(int(line))
        elif m := re.fullmatch(r"\s*(\d+)\s*([+*])\s*", line):
            current.append(int(m.group(1)))
            if m.group(2) == "*":
                ans.append(functools.reduce(operator.mul, current, 1))
            else:
                ans.append(sum(current))
            current = []
    return sum(ans)


if __name__ == "__main__":
    data = aoc_util.get_data_lines(6)
    print(part1(data))
    print(part2(data))
