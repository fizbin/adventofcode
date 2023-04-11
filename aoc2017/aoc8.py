import sys
import re
import collections

infile = "aoc8.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)


class Register:
    def __init__(self) -> None:
        self.val = 0

    def inc(self, val):
        self.val += val
        return self.val

    def dec(self, val):
        self.val -= val
        return self.val


reg = collections.defaultdict(Register)
imax = 0
for line in data:
    cmd = re.sub(
        r"(\w+) (inc|dec) (\S+) if (\w+)(.*)",
        r"reg['\1'].\2(\3) if reg['\4'].val \5 else 0",
        line,
    )
    nval = eval(cmd, {}, {"reg": reg})
    imax = max(imax, nval)
print(max(x.val for x in reg.values()))
print(imax)
