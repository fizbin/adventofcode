#!/usr/bin/python

import aoc_util
import itertools
import collections
import re
import copy
from typing import Optional

data = aoc_util.get_data_lines(25)

components: dict[str, int] = {}
connections: dict[str, list[str]] = {}
edges = []

for line in data:
    m1 = re.match(r"(\w+): (.*)", line)
    assert m1, f"Parse errer on {line}"
    von = m1.group(1)
    zus = m1.group(2).split()
    components.setdefault(von, len(components))
    vonlist = connections.setdefault(von, [])
    for zu in zus:
        components.setdefault(zu, len(components))
        connections.setdefault(zu, []).append(von)
        vonlist.append(zu)
        edges.append((von, zu))


def connected_components(avoiding):
    equivs = copy.copy(components)
    working = set(components)
    while working:
        nworking = set()
        for w in working:
            minval = min(
                equivs[nb]
                for nb in connections[w]
                if (w, nb) not in avoiding
                if (nb, w) not in avoiding
            )
            if minval != equivs[w]:
                equivs[w] = minval
                nworking.update(nb for nb in connections if equivs[nb] > minval)
        working = nworking
    counter = collections.Counter(equivs.values())
    return [x[1] for x in counter.most_common()]


TiedTuple = None | tuple[str, "TiedTuple"]


def path_from_to(von, zu, avoiding) -> Optional[list[str]]:
    been_there = set([von])
    working: list[tuple[str, TiedTuple]] = [(von, None)]
    while working:
        w = working.pop(0)
        path: TiedTuple
        (spot, path) = w
        been_there.add(spot)
        if spot == zu:
            trip = [spot]
            while path is not None:
                trip.append(path[0])
                path = path[1]
            return list(reversed(trip))
        npath = (spot, path)
        working.extend(
            (nb, npath)
            for nb in connections[spot]
            if (spot, nb) not in avoiding
            if (nb, spot) not in avoiding
            if nb not in been_there
        )
    return None


potential_weak_edges = set(edges)
weak_edges = []
for v, z in edges:
    if (v, z) not in potential_weak_edges:
        continue
    avoiding = set([(v, z)])
    path_one = path_from_to(v, z, avoiding=avoiding)
    if path_one is None:
        print("ERROR: none path1", (v, z))
        continue
    avoiding.update(itertools.pairwise(path_one))
    path_two = path_from_to(v, z, avoiding=avoiding)
    if path_two is None:
        print("ERROR: none path2", (v, z))
        continue
    avoiding.update(itertools.pairwise(path_two))
    path_three = path_from_to(v, z, avoiding=avoiding)
    if path_three is None:
        # print("DBG: none path3", (v, z))
        potential_weak_edges.intersection_update(
            set(itertools.pairwise(path_one))
            | set(itertools.pairwise(reversed(path_one)))
            | set(itertools.pairwise(path_two))
            | set(itertools.pairwise(reversed(path_two)))
        )
        weak_edges.append((v, z))
        continue

for three_edges in itertools.combinations(weak_edges, 3):
    comps = connected_components(three_edges)
    if len(comps) == 2:
        # print("DBG: found", three_edges)
        print(comps[0] * comps[1])
        break
