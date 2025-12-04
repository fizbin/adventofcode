import aoc_util


def part1(data):
    ans = []
    for x in range(len(data)):
        row = data[x]
        for y in range(len(row)):
            ch = row[y]
            if ch == "@":
                s = 0
                for xoff in (-1, 0, 1):
                    for yoff in (-1, 0, 1):
                        if (
                            (0 <= x + xoff < len(data))
                            and (0 <= y + yoff < len(row))
                            and "@" == data[x + xoff][y + yoff]
                        ):
                            s += 1
                if s <= 4:
                    ans.append((x, y))
    return ans


def part2(data):
    ans = 0
    rm_ed = part1(data)
    while rm_ed:
        for x, y in rm_ed:
            if data[x][y] == "@":
                data[x][y] = "x"
                ans += 1
        rm_ed = part1(data)
    return ans


if __name__ == "__main__":
    data = aoc_util.chargrid(aoc_util.get_data(4))
    print(len(part1(data)))
    print((part2(data)))
