import sys
import re

with open('aoc8.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

code_len = 0
data_len = 0
ubercode_len = 0

for ln in data:
    ln = ln.strip()
    code_len += len(ln)
    data_len += len(ln) - 2
    ubercode_len += len(ln) + 4
    for m in re.finditer(r'[\\](?:([\\])|(")|(x..))', ln):
        if m.group(1):
            data_len -= 1
            ubercode_len += 1
        if m.group(2):
            data_len -= 1
            ubercode_len += 1
        if m.group(3):
            data_len -= 3
        ubercode_len += 1

print((data_len, code_len))
print(code_len - data_len)

print(ubercode_len)
print(ubercode_len - code_len)
