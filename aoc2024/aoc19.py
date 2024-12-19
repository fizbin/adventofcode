from aoc_util import get_data_paras
import re
import functools

(towelstr, pat_targets) = get_data_paras(19)

towels = [x.strip() for x in towelstr.split(",")]

towelre = re.compile("^(" + "|".join(towels) + ")+$")

good_pats = []
for pat in pat_targets.splitlines():
    if towelre.match(pat):
        good_pats.append(pat)

print("Part 1:", len(good_pats))

def mk_finder():
    @functools.cache
    def find_combos(tgt: str):
        if tgt == "":
            return 1
        total = 0
        for towel in towels:
            if tgt.startswith(towel):
                total += find_combos(tgt[len(towel):])
        return total
    return find_combos

gtot = 0
for good in good_pats:
    fndr = mk_finder()
    gtot += fndr(good)

print("Part 2:", gtot)
