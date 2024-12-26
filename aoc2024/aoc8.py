from aoc_util import get_data_lines
import itertools

data = get_data_lines(8)

antennas: dict[str, list[tuple[int, int]]] = {}
width = len(data[0])
height = len(data)
for idx, line in enumerate(data):
    for cidx, char in enumerate(line):
        if char != ".":
            antennas.setdefault(char, []).append((idx, cidx))

antinodes = set()
for color, locs in antennas.items():
    for spot1, spot2 in itertools.combinations(locs, 2):
        antinode1 = (2 * spot1[0] - spot2[0], 2 * spot1[1] - spot2[1])
        antinode2 = (2 * spot2[0] - spot1[0], 2 * spot2[1] - spot1[1])
        if (0 <= antinode1[0] < height) and (0 <= antinode1[1] < width):
            antinodes.add(antinode1)
        if (0 <= antinode2[0] < height) and (0 <= antinode2[1] < width):
            antinodes.add(antinode2)

print("Part 1:", len(antinodes))

antinodes = set()
for color, locs in antennas.items():
    for spot1, spot2 in itertools.combinations(locs, 2):
        anode = spot1
        while (0 <= anode[0] < height) and (0 <= anode[1] < width):
            antinodes.add(anode)
            anode = (anode[0] + spot1[0] - spot2[0], anode[1] + spot1[1] - spot2[1])
        anode = spot2
        while (0 <= anode[0] < height) and (0 <= anode[1] < width):
            antinodes.add(anode)
            anode = (anode[0] + spot2[0] - spot1[0], anode[1] + spot2[1] - spot1[1])

print("Part 2:", len(antinodes))
