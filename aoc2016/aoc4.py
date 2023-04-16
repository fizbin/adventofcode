import sys
import collections
import re


infile = "aoc4.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

realrooms = []
retval = 0
for line in data:
    m = re.match(r"([a-z-]+)(\d+)\[(\w+)\]", line)
    if m is None:
        print(line)
    count = collections.Counter(m.group(1).replace("-", ""))
    order = sorted((-cnt, val) for (val, cnt) in count.items())
    chksum = ''.join(x for (_, x) in order[:5])
    if chksum == m.group(3):
        retval += int(m.group(2))
        realrooms.append(line)
print(retval)

for line in realrooms:
    m = re.match(r"([a-z-]+)(\d+)\[(\w+)\]", line)
    outname = ''
    sector_id = int(m.group(2))
    rot = sector_id % 26
    for char in m.group(1):
        if char.isalpha():
            if chr(ord(char) + rot).isalpha():
                outname += chr(ord(char) + rot)
            else:
                outname += chr(ord(char) - 26 + rot)
        else:
            outname += char
    if 'north' in outname:
        print(outname, sector_id)