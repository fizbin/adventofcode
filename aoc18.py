"""
Advent of code day 18 in python.
"""

import sys
import re
import functools
import collections
import itertools

def find_nested4(snum):
    # returns number of chars, or None
    count = 0
    for (idx, char) in enumerate(snum):
        if char == '[':
            if count == 4:
                return idx
            count += 1
        if char == ']':
            count -= 1
    return None

def process_snumber(snum):
    m1 = None
    n4 = find_nested4(snum)
    if n4 is not None:
        m1 = re.match(f'({"." * n4})(\\[(\\d+),(\\d+)\\])(.*)', snum)
    if m1:
        # explode!
        lfttxt = m1.group(1)
        rgttxt = m1.group(5)
        lft = re.match(r'(.*\D)(\d+)(\D*)$', lfttxt)
        rgt = re.match(r'(\D*?)(\d+)(.*)$', rgttxt)
        if lft:
            lfttxt = lft.group(1) + str(int(lft.group(2)) + int(m1.group(3))) + lft.group(3)
        if rgt:
            rgttxt = rgt.group(1) + str(int(rgt.group(2)) + int(m1.group(4))) + rgt.group(3)
        return lfttxt + '0' + rgttxt
    m2 = re.match(r'(.*?)(\d\d+)(.*)', snum)
    if m2:
        rnum = int(m2.group(2))
        lft = rnum // 2
        rgt = (rnum+1) // 2
        return f"{m2.group(1)}[{lft},{rgt}]{m2.group(3)}"
    return snum

def full_process(snum):
    x = process_snumber(snum)
    while x != snum:
        snum = x 
        x = process_snumber(snum)
    return snum

def add_snum(snum1, snum2):
    return full_process(f"[{snum1},{snum2}]")

def add_snum_list(snumlist):
    current = snumlist[0]
    rest = snumlist[1:]
    while rest:
        current = add_snum(current, rest[0])
        if '[]' in current:
            raise Exception(f"yo: {current} {rest[0]}")
        rest = rest[1:]
    return current

def magnitude(snum):
    m1 = re.match(r'(.*)(\[(\d+),(\d+)\])(.*)', snum)
    while m1:
        snum = f"{m1.group(1)}{3*int(m1.group(3))+2*int(m1.group(4))}{m1.group(5)}"
        m1 = re.match(r'(.*)(\[(\d+),(\d+)\])(.*)', snum)
    return snum

if __name__ == '__main__':
    with open("aoc18.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        # data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
        # data = re.findall(r"(\w+)-(\w+)", f.read())
        # data = re.findall(r"(-?\d+)\.\.(-?\d+)", f.read())
        data = re.findall(r"\S+", f.read())
        # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
        # data = [tuple(int(y) for y in tup) for tup in data]
    tot = add_snum_list(data)
    mag = magnitude(tot)
    print(mag)
    maxmag = 0
    for idx1 in range(len(data)):
        for idx2 in range(len(data)):
            mag = magnitude(add_snum(data[idx1], data[idx2]))
            maxmag = max([maxmag, int(mag)])
    print(maxmag)