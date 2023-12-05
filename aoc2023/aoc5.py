#!/usr/bin/env python

from aoc_util import get_data_paras, numbers, chunked

data = get_data_paras(5)
lowest_location = 10**200

seeds = numbers(data[0])

for mapper in data[1:]:
    new_seeds = list(seeds)
    for mapline in mapper.splitlines()[1:]:
        (dstart, sstart, rlen) = numbers(mapline)
        for idx, seed in enumerate(seeds):
            if seed >= sstart and seed < sstart + rlen:
                new_seeds[idx] = seed + dstart - sstart
    seeds = new_seeds

print(min(seeds))


def apply_map(srange, dstart, sstart, rlen):
    """
    range -> ([remaining unmapped input ranges], [output ranges])
    """
    if srange[1] <= sstart:
        return ([srange], [])
    if srange[0] >= sstart + rlen:
        return ([srange], [])
    if srange[0] < sstart:
        (subin, subout) = apply_map((sstart, srange[1]), dstart, sstart, rlen)
        return ([(srange[0], sstart)] + subin, subout)
    if srange[1] > sstart + rlen:
        scutoff = sstart + rlen
        return ([(scutoff, srange[1])], [(srange[0] + dstart - sstart, scutoff + dstart - sstart)])
    return ([], [(srange[0] + dstart - sstart, srange[1] + dstart - sstart)])


in_sranges = [(a, a + rlen) for (a, rlen) in chunked(2, numbers(data[0]))]
# count0 = sum(b - a for (a, b) in in_sranges)
# print(f"Count is {count0}")
for mapper in data[1:]:
    out_sranges = []
    for mapline in mapper.splitlines()[1:]:
        (dstart, sstart, rlen) = numbers(mapline)
        new_in_sranges = []
        for srange in in_sranges:
            (new_in, new_out) = apply_map(srange, dstart, sstart, rlen)
            # print(f"{srange}, {dstart}, {sstart}, {rlen} -> ({new_in}, {new_out})")
            out_sranges.extend(new_out)
            new_in_sranges.extend(new_in)
        in_sranges = new_in_sranges
    in_sranges += out_sranges
    # print(repr(in_sranges))
    # count = sum(b - a for (a, b) in in_sranges)
    # print(f"Count is {count}")
    # if count != count0:
    #     breakpoint()

print(min(a for (a, b) in in_sranges))
