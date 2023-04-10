#!/usr/bin/env python3
import sys
import time
import re

with open('aoc4.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(line.strip() for line in f)


data1 = []
working = ''
for line in data:
    if not line:
        data1.append(working)
        working = ''
    else:
        if working:
            working += ' '
        working += line
if working:
    data1.append(working)

required = set('''byr
iyr 
eyr 
hgt 
hcl 
ecl 
pid
cid'''.split())

nvalidA = 0
nvalidB = 0
for passp in data1:
    found = {}
    for match in re.finditer(r'(\S+):(\S+)', passp):
        found[match.group(1)] = match.group(2)
    if set(found.keys()) | {'cid'} == required:
        nvalidA += 1
    else:
        continue
    try:
        byr = int(found['byr'])
        iyr = int(found['iyr'])
        eyr = int(found['eyr'])
        if byr < 1920 or byr > 2002:
            continue
        if iyr < 2010 or iyr > 2020:
            continue
        if eyr > 2030 or eyr < 2020:
            continue
        if found['hgt'].endswith('cm'):
            hgt = int(found['hgt'][:-2])
            if hgt < 150 or hgt > 193:
                continue
        elif found['hgt'].endswith('in'):
            hgt = int(found['hgt'][:-2])
            if hgt < 59 or hgt > 76:
                continue
        else:
            continue
        if not re.match(r'^#[0-9a-f]{6}$', found['hcl']):
            continue
        if found['ecl'] not in 'amb blu brn gry grn hzl oth'.split():
            continue
        if not re.match(r'^\d{9}$', found['pid']):
            continue
        nvalidB += 1
    except (ValueError, KeyError):
        continue


print(nvalidA)
print(nvalidB)
