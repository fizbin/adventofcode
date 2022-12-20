from aoc_util import *
import numpy as np

def doshuffle(data, order):
    ldata = len(data)
    for (idx, val) in enumerate(data):
        curridx = order[idx]
        rval = val % (ldata - 1)
        secondidx = curridx + rval
        if secondidx >= ldata:
            secondidx = curridx - (ldata - 1) + rval
        if secondidx == curridx:
            continue
        if secondidx < curridx:
            order = np.where((secondidx <= order) & (order < curridx), order + 1, order)
        elif curridx < secondidx:
            order = np.where((curridx < order) & (order <= secondidx), order - 1, order)
        order[idx] = secondidx
        # assert len(set(order)) == ldata
    return order


def part1(data):
    ldata = len(data)
    order = np.arange(ldata, dtype=np.int64)
    order = doshuffle(data, order)
    orderinv = np.copy(order)
    orderinv[order] = np.arange(ldata, dtype=np.int64)

    zeroidx = data.index(0)
    zerospot = order[zeroidx]
    tot = 0
    for off in (1000, 2000, 3000):
        sp = (off + zerospot) % ldata
        tot += data[orderinv[sp]]

    print(tot)

def part2(data):
    bigdata = [811589153*d for d in data]
    ldata = len(data)
    order = np.arange(ldata, dtype=np.int64)
    for _ in range(10):
        order = doshuffle(bigdata, order)
    orderinv = np.copy(order)
    orderinv[order] = np.arange(ldata, dtype=np.int64)

    zeroidx = data.index(0)
    zerospot = order[zeroidx]
    tot = 0
    for off in (1000, 2000, 3000):
        sp = (off + zerospot) % ldata
        tot += bigdata[orderinv[sp]]

    print(tot)

def main():
    data = numbers(get_data(20))

    # data = numbers("""
    # 1
    # 2
    # -3
    # 3
    # -2
    # 0
    # 4
    # """)

    part1(data)
    part2(data)

if __name__ == '__main__':
    main()