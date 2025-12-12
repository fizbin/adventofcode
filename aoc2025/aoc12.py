import aoc_util
import numpy as np
import re


def presents_fit(pdict, width, height, req):
    p_sz = {i: p.sum() for (i,p) in pdict.items()}
    req_sz = sum(v * p_sz[i] for (i, v) in enumerate(req))
    if req_sz > width * height:
        return False
    # TODO: determine if the presents can be twisted into the shape
    # given, after checking total size
    return True


if __name__ == "__main__":
    paras = aoc_util.get_data_paras(12)
    present_data = paras[:-1]
    regions = paras[-1].strip().split("\n")
    presents = {}
    for pdata in present_data:
        m = re.fullmatch(r"(\d+):\n((?:[#.]+\s*)+)", pdata)
        assert m
        tgrid = np.array(aoc_util.chargrid(m.group(2)), dtype=str)
        presents[int(m.group(1))] = (tgrid == '#').astype(int)
    can_fit = 0
    for region_line in regions:
        m = re.fullmatch(r"(\d+)x(\d+): ([\d ]+)", region_line)
        assert m
        present_req = [int(x) for x in m.group(3).strip().split()]
        if presents_fit(presents, int(m.group(1)), int(m.group(2)), present_req):
            can_fit += 1
    print(can_fit)
