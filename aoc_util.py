import itertools
import re
import sys
from typing import List

import numpy as np


def get_data(problem_num: int) -> str:
    infilename = f"aoc{problem_num}.in"
    if len(sys.argv) >= 2:
        infilename = sys.argv[1]
    with open(infilename, encoding="utf-8") as infile:
        return infile.read()


def get_data_lines(problem_num: int) -> List[str]:
    return get_data(problem_num).splitlines()


def get_data_paras(problem_num: int) -> List[str]:
    return [
        x if x.endswith("\n") else f"{x}\n" for x in get_data(problem_num).split("\n\n")
    ]


def numbers(in_string, is_hex=False) -> List[int]:
    regex = r"\b\d+\b"
    base = 10
    if is_hex:
        regex = r"\b[0-9A-Fa-f]+\b"
        base = 16
    return [int(x, base) for x in re.findall(regex, in_string)]


def chargrid(in_string: str) -> List[List[str]]:
    return [list(line) for line in in_string.splitlines()]


def get_rotations(ndims, reflections=False, dtype=None):
    idmat = np.identity(ndims, dtype=dtype)
    retval = [idmat]
    for (ax1, ax2) in itertools.combinations(range(ndims), 2):
        newval = np.copy(idmat)
        newval[ax1] = idmat[ax2]
        newval[ax2] = -idmat[ax1]
        retval.append(newval)
        newval = np.copy(idmat)
        newval[ax1] = -idmat[ax2]
        newval[ax2] = idmat[ax1]
        retval.append(newval)
        newval = np.copy(idmat)
        newval[ax1] = -idmat[ax1]
        newval[ax2] = -idmat[ax2]
        retval.append(newval)
    if reflections:
        nmat = np.copy(idmat)
        nmat[0] = -idmat[0]
        retval.append(nmat)
    working = []
    while len(working) < len(retval):
        working = list(retval)
        for (mat1, mat2) in itertools.combinations_with_replacement(working, 2):
            prodmat = mat1 @ mat2
            if not any((z == prodmat).all() for z in retval):
                retval.append(prodmat)
    return retval
