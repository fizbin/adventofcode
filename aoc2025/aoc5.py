import aoc_util
import re


def get_fresh_stuff(fresh, ids):
    fresh_ranges = []
    for m in re.finditer(r"(\d+)-(\d+)", fresh):
        (start, end) = (int(m.group(1)), int(m.group(2)))
        fresh_ranges.append((start, end))
    fresh_ranges.sort()
    fresh_ids = set()
    for m in re.finditer(r"(\d+)", ids):
        idval = int(m.group(1))
        for start, end in fresh_ranges:
            if start <= idval <= end:
                fresh_ids.add(idval)
    fresh_ranges.sort()
    tot_all_fresh = 0
    cur_range = list(fresh_ranges[0])
    for start, end in fresh_ranges[1:]:
        if start > cur_range[1] + 1:
            tot_all_fresh += cur_range[1] - cur_range[0] + 1
            cur_range = [start, end]
        else:
            cur_range[1] = max(end, cur_range[1])
    tot_all_fresh += cur_range[1] - cur_range[0] + 1
    return (fresh_ids, tot_all_fresh)


if __name__ == "__main__":
    (fresh, ids) = aoc_util.get_data_paras(5)
    (fresh, ans2) = get_fresh_stuff(fresh, ids)
    print(len(fresh))
    print(ans2)
