import aoc_util
import functools
import itertools
import re


def part1(connections: dict[str, list[str]]) -> int:
    rev_map: dict[str, list[str]] = {}
    for x, ys in connections.items():
        for y in ys:
            rev_map.setdefault(y, []).append(x)
    tot = {"out": 1}
    changed = {"out"}
    while changed:
        ochanged = changed
        changed = set()
        for y in (nb for ch in ochanged for nb in rev_map.get(ch, [])):
            if y not in tot:
                if all(nb in tot for nb in connections[y]):
                    tot[y] = sum(tot[nb] for nb in connections[y])
                    changed.add(y)
    return tot["you"]


def part2(connections: dict[str, list[str]]) -> int:
    rev_map: dict[str, list[str]] = {}
    for x, ys in connections.items():
        for y in ys:
            rev_map.setdefault(y, []).append(x)
    tot: dict[str, tuple[int, ...]] = {
        "out": (1, 0, 0, 0)
    }  # paths touching neither, just dac, just fft, visit both
    changed = {"out"}
    while changed:
        ochanged = changed
        changed = set()
        for y in (nb for ch in ochanged for nb in rev_map.get(ch, [])):
            if y not in tot:
                if all(nb in tot for nb in connections[y]):
                    ptot = [0, 0, 0, 0]
                    for nb in connections[y]:
                        if nb == "dac":
                            ptot[1] += tot[nb][0] + tot[nb][1]
                            ptot[3] += tot[nb][2] + tot[nb][3]
                        elif nb == "fft":
                            ptot[2] += tot[nb][0] + tot[nb][2]
                            ptot[3] += tot[nb][1] + tot[nb][3]
                        else:
                            ptot[0] += tot[nb][0]
                            ptot[1] += tot[nb][1]
                            ptot[2] += tot[nb][2]
                            ptot[3] += tot[nb][3]
                    tot[y] = tuple(ptot)
                    changed.add(y)
    return tot["svr"][3]


if __name__ == "__main__":
    datalines = aoc_util.get_data_lines(11)
    connections = {}
    parser = re.compile(r"(\S+): *(.*)")
    for line in datalines:
        m = parser.fullmatch(line)
        assert m is not None
        connections[m.group(1)] = m.group(2).split()
    print(part1(connections))
    print(part2(connections))
