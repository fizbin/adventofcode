import itertools
import re
import sys
from typing import List, Callable, Optional, Sequence, Set, Tuple, TypeVar
import heapq

import numpy as np


X = TypeVar("X")
Y = TypeVar("Y")


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


def chunked(n, iterable, enforce_size=True):
    for _, subit in itertools.groupby(enumerate(iterable), key=lambda x: x[0] // n):
        rettup = tuple(x[1] for x in subit)
        if len(rettup) == n:
            yield rettup
        else:
            assert not enforce_size, f"{len(rettup)} left over in iteration"


def rolling(n, iterable):
    yieldval = ()
    for thing in iterable:
        if len(yieldval) < n:
            yieldval = yieldval + (thing,)
        else:
            yieldval = yieldval[1:] + (thing,)
        if len(yieldval) == n:
            yield yieldval


def astar(
    start: X,
    goalf: Callable[[X], bool],
    neighborf: Callable[[X], Sequence[Tuple[X, int]]],
    estimatef: Callable[[X], int] = lambda _: 0,
) -> Optional[Tuple[int, List[X]]]:
    visited: Set[X] = set()
    workq: List[Tuple[int, int, Tuple, X]]
    workq = [(0, 0, (), start)]
    while workq:
        (_, cost_so_far, path_tup, pos) = heapq.heappop(workq)
        visited.add(pos)
        if goalf(pos):
            path = []
            while len(path_tup) == 2:
                (a, b) = path_tup
                path.append(a)
                path_tup = b
            path.append(start)
            path.reverse()
            return (cost_so_far, path)
        for (next_spot, dist) in neighborf(pos):
            if next_spot not in visited:
                estimate = estimatef(next_spot)
                heapq.heappush(
                    workq,
                    (
                        cost_so_far + estimate + dist,
                        cost_so_far + dist,
                        (next_spot, path_tup),
                        next_spot,
                    ),
                )
    return None


def numpy_shift(array, shiftsz, axis, fill):
    foo = np.roll(array, shift=shiftsz, axis=axis)
    if shiftsz != 0:
        maxidx = np.shape(foo)[axis]
        filltup = [slice(None)] * len(np.shape(foo))
        if abs(shiftsz) >= maxidx:
            foo[tuple(filltup)] = fill
        else:
            if shiftsz > 0:
                for target in range(shiftsz):
                    filltup[axis] = target
                    foo[tuple(filltup)] = fill
            else:
                for target in range(shiftsz, 0):
                    filltup[axis] = target
                    foo[tuple(filltup)] = fill
    return foo
