import itertools

from aoc_util import get_data_lines

data = get_data_lines(23)

connections = set(data)
nodes = set(y.split("-")[0] for y in data)
tnodes = set(y for y in nodes if y[0] == "t")

congr = {}
for con in connections:
    (von, zu) = sorted(con.split("-"), key=lambda s: (s[0] != "t", s))
    congr[von] = sorted(congr.get(von, []) + [zu], key=lambda s: (s[0] != "t", s))

total = 0
for tnode in tnodes:
    rest = congr.get(tnode, [])
    for a, b in itertools.combinations(rest, 2):
        if b in congr.get(a, []):
            total += 1

print("Part 1:", total)


def is_fully_connected(candidates):
    cs = sorted(candidates, key=lambda s: (s[0] != "t", s))
    for xidx, x in enumerate(cs[:-1]):
        xc = congr.get(x, [])
        for y in cs[xidx + 1 :]:
            if y not in xc:
                return False
    return True


biggest_sofar = None
big_size = 0
for node, downstream in sorted(congr.items(), key=lambda s: (s[0][0] != "t", s[0])):
    for node2 in downstream:
        candidates = set(downstream) & set(congr.get(node2, []))
        candidates.update([node, node2])
        if len(candidates) > big_size:
            for drop_num in range(len(candidates) - big_size):
                found_s = False
                for candidates_ in itertools.combinations(candidates, len(candidates) - drop_num):
                    if is_fully_connected(candidates_):
                        if len(candidates_) > big_size:
                            biggest_sofar = candidates_
                            big_size = len(biggest_sofar)
                            # print("N", big_size)
                        found_s = True
                        break
                if found_s:
                    break

print("Part 2:", ",".join(sorted(biggest_sofar)))
