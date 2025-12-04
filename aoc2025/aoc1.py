import aoc_util


def part1(data):
    ans = 0
    dial = 50
    for line in data:
        (direction, distance) = (line[0:1], int(line[1:]))
        if direction == "R":
            dial = (dial + distance) % 100
        else:
            dial = (dial - distance) % 100
        if dial == 0:
            ans += 1
    return ans


def part2(data):
    ans = 0
    dial = 50
    for line in data:
        (direction, tdistance) = (line[0:1], int(line[1:]))
        while tdistance > 0:
            distance = min(tdistance, 100)
            tdistance -= distance
            if direction == "R":
                if dial + distance > 100:
                    ans += 1
                dial = (dial + distance) % 100
            else:
                if dial > 0 and dial - distance < 0:
                    ans += 1
                dial = (dial - distance) % 100
            if dial == 0:
                ans += 1
    return ans


if __name__ == "__main__":
    data = aoc_util.get_data_lines(1)
    print(part1(data))
    print(part2(data))
