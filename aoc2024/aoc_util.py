import itertools
import re
import sys
from typing import List, Callable, Optional, Sequence, Set, Tuple, TypeVar, TYPE_CHECKING
import heapq

import numpy as np

if TYPE_CHECKING:
    from _typeshed import SupportsRichComparison

X = TypeVar("X")
Y = TypeVar("Y")
XS = TypeVar("XS", bound="SupportsRichComparison")


def get_data(problem_num: int) -> str:
    infilename = f"aoc{problem_num}.in"
    if len(sys.argv) >= 2:
        infilename = sys.argv[1]
    with open(infilename, encoding="utf-8") as infile:
        return infile.read()


def get_data_lines(problem_num: int) -> List[str]:
    return get_data(problem_num).splitlines()


def get_data_paras(problem_num: int) -> List[str]:
    return [x if x.endswith("\n") else f"{x}\n" for x in get_data(problem_num).split("\n\n")]


def numbers(in_string, is_hex=False) -> List[int]:
    regex = r"(?:(?<=\W)-)?\b\d+"
    base = 10
    if is_hex:
        regex = r"(?:(?<=\W)-)?\b(?:0x)?[0-9A-Fa-f]+"
        base = 16
    return [int(x, base) for x in re.findall(regex, " " + in_string)]


def chargrid(in_string: str) -> List[List[str]]:
    return [list(line) for line in in_string.splitlines()]


def get_rotations(ndims, reflections=False, dtype=None):
    idmat = np.identity(ndims, dtype=dtype)
    retval = [idmat]
    for ax1, ax2 in itertools.combinations(range(ndims), 2):
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
        for mat1, mat2 in itertools.combinations_with_replacement(working, 2):
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
    *,
    start: Optional[XS] = None,
    starts: Optional[Sequence[XS]] = None,
    goalf: Callable[[XS], bool],
    neighborf: Optional[Callable[[XS], Sequence[XS]]] = None,
    neighbor_distf: Optional[Callable[[XS], Sequence[Tuple[XS, int]]]] = None,
    estimatef: Callable[[XS], int] = lambda _: 0,
) -> Optional[Tuple[int, List[XS]]]:
    """
    Dijkstra/A* search. All arguments use named parameters.

    Args:
        start: Single start point. Must give one of 'start' or 'starts'.
        starts: Start points. Must give one of 'start' or 'starts'.
        goalf: Identify goal.
        neighborf: Neighbors. Must give one of 'neighbors' or 'neighbor_distf'.
                   Assumed distance "1" to all neighbors.
        neighbor_distf: (Neighbor, distance) tuples
        estimatef: Estimate of remaining dist from spot. Must be underestimate

    Returns:
        None if no path found; otherwise (total_distance, path_to_goal)
        path_to_goal[0] is the start that won; path_to_goal[-1] is the goal reached
    """
    assert (start is None) != (starts is None), "Exactly one of start, starts must be given"
    assert (neighborf is None) != (
        neighbor_distf is None
    ), "Exactly one of neighborf and neighbor_distf must be given"
    if starts is None:
        assert start is not None  # mypy
        starts = [start]
    if neighbor_distf is None:

        def default_neighbor_dist(spot):
            return [(n, 1) for n in neighborf(spot)]

        neighbor_distf = default_neighbor_dist
    visited: Set[XS] = set()
    workq: List[Tuple[int, int, Tuple, XS]]
    workq = [(0, 0, (spot, ()), spot) for spot in sorted(starts)]
    while workq:
        (_, cost_so_far, path_tup, pos) = heapq.heappop(workq)
        if pos in visited:
            continue
        visited.add(pos)
        if goalf(pos):
            path = []
            while len(path_tup) == 2:
                (car, cdr) = path_tup
                path.append(car)
                path_tup = cdr
            path.reverse()
            return (cost_so_far, path)
        for next_spot, dist in neighbor_distf(pos):
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
