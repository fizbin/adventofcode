import aoc_util


def part1(data) -> int:
    ans = set()
    start: None | tuple[int, int] = None
    for x in range(len(data)):
        for y in range(len(data[x])):
            if data[x][y] == "S":
                start = (x, y)
    assert start
    (x, y) = start
    tachy_set = set([y])
    for row_idx in range(x, len(data)):
        row = data[row_idx]
        # print(row)
        new_tachy_set = set()
        for tachy in tachy_set:
            if 0 <= tachy < len(row):
                if row[tachy] == "^":
                    new_tachy_set.add(tachy - 1)
                    new_tachy_set.add(tachy + 1)
                    ans.add((row_idx, tachy))
                else:
                    new_tachy_set.add(tachy)
            else:
                new_tachy_set.add(tachy)
        tachy_set = new_tachy_set
    return len(ans)


def part2(data) -> int:
    start: None | tuple[int, int] = None
    for x in range(len(data)):
        for y in range(len(data[x])):
            if data[x][y] == "S":
                start = (x, y)
    assert start
    (x, y) = start
    tachy_world = {y: 1}
    for row_idx in range(x, len(data)):
        row = data[row_idx]
        # print(row)
        new_world: dict[int, int] = {}
        for tachy, tmul in tachy_world.items():
            if 0 <= tachy < len(row):
                if row[tachy] == "^":
                    new_world[tachy - 1] = new_world.get(tachy - 1, 0) + tmul
                    new_world[tachy + 1] = new_world.get(tachy + 1, 0) + tmul
                else:
                    new_world[tachy] = new_world.get(tachy, 0) + tmul
        tachy_world = new_world
    return sum(tachy_world.values())


if __name__ == "__main__":
    data = aoc_util.chargrid(aoc_util.get_data(7))
    print(part1(data))
    print(part2(data))
