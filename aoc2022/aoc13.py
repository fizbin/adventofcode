from aoc_util import get_data_paras
from functools import cmp_to_key
import json

data = get_data_paras(13)


def is_right_order(l1, l2):
    if isinstance(l1, int) and isinstance(l2, int):
        if l1 < l2:
            return True
        if l2 < l1:
            return False
        return None
    if isinstance(l1, list) and isinstance(l2, list):
        if l1 == []:
            if l2 == []:
                return None
            return True
        if l2 == []:
            return False
        c1 = is_right_order(l1[0], l2[0])
        if c1 is not None:
            return c1
        return is_right_order(l1[1:], l2[1:])
    if isinstance(l1, int):
        return is_right_order([l1], l2)
    if isinstance(l2, int):
        return is_right_order(l1, [l2])
    assert False, f"{repr((l1, l2))}"


tot = 0
for (idx, dpara) in enumerate(data):
    ridx = idx + 1
    (sval1, sval2) = dpara.splitlines()
    # if ridx == 3: breakpoint()
    val1 = json.loads(sval1)
    val2 = json.loads(sval2)
    if is_right_order(val1, val2):
        tot += ridx
print(tot)


def wcmp(it1, it2):
    if it1 == it2:
        return 0
    if is_right_order(it1, it2):
        return -1
    return 1


allpackets = [json.loads(dstr) for dpara2 in data for dstr in dpara2.splitlines()] + [
    [[2]],
    [[6]],
]
allpackets.sort(key=cmp_to_key(wcmp))
# print("\n".join(str(ap) for ap in allpackets))
print((allpackets.index([[2]]) + 1) * (allpackets.index([[6]]) + 1))
