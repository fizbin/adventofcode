import sys
import re
import itertools

with open('aoc3.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = re.findall(r'\d+', f.read())

counts = [0] * (len(data[0]))
for x in data:
    for (idx, n) in enumerate(x):
        counts[idx] += 1 if n == '1' else 0

midpoint = len(data) / 2
total1s = ['1' if c > midpoint else '0' for c in counts]
total1int = int(''.join(total1s), 2)
total2s = ['0' if c > midpoint else '1' for c in counts]
total2int = int(''.join(total2s), 2)
print(total1int*total2int)

def winnow(datas, pos, rule):
    count = 0
    for x in datas:
        count += 1 if x[pos] == '1' else 0
    tgt = '0' if rule == 0 else '1'
    midp = len(datas) / 2
    if rule == 0:
        if count >= midp:
            tgt = '1'
        else:
            tgt = '0'
    if rule == 1:
        if count >= midp:
            tgt = '0'
        else:
            tgt = '1'
    return [x for x in datas if x[pos] == tgt]
print()
data2 = data
pos = 0
while len(data2) > 1:
    data2 = winnow(data2, pos, 0)
    pos += 1
o2gen = int(''.join(data2[0]), 2)
data2 = data
pos = 0
while len(data2) > 1:
    data2 = winnow(data2, pos, 1)
    pos += 1
co2scr = int(''.join(data2[0]), 2)

print(o2gen*co2scr)