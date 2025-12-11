import aoc_util
import functools
import itertools
import re


def part1(data: list[tuple[int, ...]]) -> int:
    points = set(data)
    points_tl: set[tuple[int, ...]] = set()
    points_tr: set[tuple[int, ...]] = set()
    points_bl: set[tuple[int, ...]] = set()
    points_br: set[tuple[int, ...]] = set()
    for point in points:
        if not any(p[0] <= point[0] and p[1] <= point[1] for p in points_tl):
            points_tl.add(point)
        if not any(p[0] >= point[0] and p[1] <= point[1] for p in points_bl):
            points_bl.add(point)
        if not any(p[0] <= point[0] and p[1] >= point[1] for p in points_tr):
            points_tr.add(point)
        if not any(p[0] >= point[0] and p[1] >= point[1] for p in points_br):
            points_br.add(point)
    # print("DBG> ", len(data), len(points_tr))
    maxarea = 0
    for p1 in points_tl:
        for p2 in points_br:
            maxarea = max(maxarea, (abs(p1[0] - p2[0]) + 1) * (abs(p1[1] - p2[1]) + 1))
    for p1 in points_tr:
        for p2 in points_bl:
            maxarea = max(maxarea, (abs(p1[0] - p2[0]) + 1) * (abs(p1[1] - p2[1]) + 1))
    return maxarea


def acceptable(p1, p2, ranges_by_x_coord: dict[int, list[tuple[int, int]]]):
    for x in range(min(p1[0], p2[0]), max(p1[0], p2[0]) + 1):
        if x not in ranges_by_x_coord:
            continue
        (ymin, ymax) = (min(p1[1], p2[1]), max(p1[1], p2[1]))
        for rmin, rmax in ranges_by_x_coord[x]:
            if rmin <= ymin <= rmax:
                ymin = rmax + 1
            if rmin <= ymax <= rmax:
                ymax = rmin - 1
        if ymin <= ymax:
            return False
    return True


def part2(data: list[tuple[int, ...]]) -> int:
    allpath: list[tuple[tuple[int, ...], str]] = []
    point_x_coords: set[int] = set()
    for idx in range(len(data)):
        p1 = data[idx]
        p2 = data[(idx + 1) % len(data)]
        point_x_coords.add(p1[0])
        # if p1 == (7,3):
        #     breakpoint()
        if p1[0] == p2[0] and p1[1] <= p2[1]:
            allpath.extend([((p1[0], y), "=") for y in range(p1[1] + 1, p2[1])])
        elif p1[0] == p2[0] and p1[1] >= p2[1]:
            allpath.extend([((p1[0], y), "=") for y in range(p2[1] + 1, p1[1])])
        elif p1[1] == p2[1] and p1[0] <= p2[0]:
            allpath.extend([((y, p1[1]), "+") for y in range(p1[0], p2[0] + 1)])
        elif p1[1] == p2[1] and p1[0] >= p2[0]:
            allpath.extend([((y, p1[1]), "-") for y in range(p2[0], p1[0] + 1)])
        else:
            raise ValueError(f"{p1} -> {p2}")
    ranges_by_x_coord: dict[int, list[tuple[int, int]]] = {}
    path_by_x_coord: dict[int, list[tuple[int, str]]] = {}
    for p in allpath:
        path_by_x_coord.setdefault(p[0][0], []).append((p[0][1], p[1]))
    prev_had_points = True
    for x in sorted(path_by_x_coord):
        we_have_points = x in point_x_coords
        if not we_have_points and not prev_had_points:
            continue
        prev_had_points = we_have_points
        ranges: list[tuple[int, int]] = []
        cur_left = None
        last_pm = None
        for y, pm in sorted(path_by_x_coord[x]):
            # print("DBG4>", (x, y, pm))
            if pm == "+" and last_pm != "=":
                assert cur_left is None
                cur_left = y
            if pm == "-":
                assert cur_left is not None
                ranges.append((cur_left, y))
                cur_left = None
            if pm == "=" and last_pm == "-":
                assert cur_left is None
                cur_left = ranges[-1][0]
                ranges[-1:] = []
            last_pm = pm
        ranges_by_x_coord[x] = ranges
    maxarea = 0
    # print("DBG3>", ranges_by_x_coord)
    for idx, p1 in enumerate(data):
        for p2 in data[idx + 1 :]:
            myarea = (abs(p1[0] - p2[0]) + 1) * (abs(p1[1] - p2[1]) + 1)
            if myarea > maxarea and acceptable(p1, p2, ranges_by_x_coord):
                maxarea = myarea
                # print("DBG2>", maxarea, p1, p2)
            elif myarea > maxarea:
                pass
                # print("DBG2!", myarea, p1, p2)
    return maxarea


if __name__ == "__main__":
    datalines = aoc_util.get_data_lines(9)
    datapoints = [tuple(int(x) for x in line.split(",")) for line in datalines]
    print(part1(datapoints))
    print(part2(datapoints))
