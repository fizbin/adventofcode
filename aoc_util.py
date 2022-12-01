import numpy as np
import itertools
import sys

def get_data(problem_num):
    infilename = f"aoc{problem_num}.in"
    if len(sys.argv) >= 2:
        infilename = sys.argv[1]
    with open(infilename, encoding="utf-8") as infile:
        return infile.read()

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
    if reflections:
        nmat = np.copy(idmat)
        nmat[0] = -idmat[0]
        retval.append(nmat)
    working = []
    while len(working) < len(retval):
        working = list(retval)
        for (mat1, mat2) in itertools.combinations_with_replacement(working, 2):
            y = mat1 @ mat2
            if not any((z == y).all() for z in retval):
                retval.append(y)
    return retval
