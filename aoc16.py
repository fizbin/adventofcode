"""
Advent of code day 16 in python.
"""

import sys
import re
import collections
import operator

with open("aoc16.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
    # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    # data = re.findall(r"(\w+)-(\w+)", f.read())
    data = re.findall(r"\S+", f.read())[0]
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

bindata = "".join(bin(int(s, 16) | 32)[4:] for s in data)


def parsenextpacket(data):
    # data -> (version, remaining)
    verbin = data[:3]
    tidbin = data[3:6]
    if tidbin == "100":
        start = 6
        while data[start : start + 1] == "1":
            start += 5
        return (int(verbin, 2), data[start + 5 :])
    # else other packet
    if data[6:7] == "1":
        return (int(verbin, 2), data[18:])
    return (int(verbin, 2), data[22:])


mdata = bindata
count = 0
while mdata:
    (ver, mdata) = parsenextpacket(mdata)
    count += ver
print(count)


def get_packet_func(type):
    def myprod(vals):
        res = 1
        for v in vals:
            res *= v
        return res

    if type == 0:
        return sum
    if type == 1:
        return myprod
    if type == 2:
        return min
    if type == 3:
        return max
    if type == 5:
        return lambda a: int(a[0] > a[1])
    if type == 6:
        return lambda a: int(a[0] < a[1])
    if type == 7:
        return lambda a: int(a[0] == a[1])


def parsenextpacketval(data):
    # data -> (version, remaining)
    verbin = data[:3]
    tidbin = data[3:6]
    if tidbin == "100":
        start = 6
        bval = data[start + 1 : start + 5]
        while data[start : start + 1] == "1":
            start += 5
            bval += data[start + 1 : start + 5]
        return (int(bval, 2), data[start + 5 :])
    # else other packet
    tid = int(tidbin, 2)
    subvals = []
    if data[6:7] == "1":
        npacketsbin = data[7:18]
        npackets = int(npacketsbin, 2)
        data = data[18:]
        for _ in range(npackets):
            (nval, data) = parsenextpacketval(data)
            subvals.append(nval)
    else:
        nbitsbin = data[7:22]
        nbits = int(nbitsbin, 2)
        data = data[22:]
        sdata = data[0:nbits]
        data = data[nbits:]
        while sdata:
            (nval, sdata) = parsenextpacketval(sdata)
            subvals.append(nval)
    return (get_packet_func(tid)(subvals), data)


print(parsenextpacketval(bindata))
