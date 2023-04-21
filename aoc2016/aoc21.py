import sys
import re

infile = "aoc21.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

psswrd = list("abcdefgh")

# psswrd = list('fbhaegdc')

for line in data:
    old = list(psswrd)
    if m := re.match(r"swap position (?P<X>\d+) with position (?P<Y>\d+)", line):
        x, y = int(m.group("X")), int(m.group("Y"))
        psswrd[x], psswrd[y] = psswrd[y], psswrd[x]
    elif m := re.match(r"rotate left (?P<X>\d+) steps?", line):
        x = int(m.group("X"))
        psswrd = psswrd[x:] + psswrd[:x]
    elif m := re.match(r"rotate right (?P<X>\d+) steps?", line):
        x = int(m.group("X"))
        psswrd = psswrd[-x:] + psswrd[:-x]
    elif m := re.match(r"reverse positions (?P<X>\d+) through (?P<Y>\d+)", line):
        x, y = int(m.group("X")), int(m.group("Y"))
        psswrd[x : y + 1] = list(reversed(psswrd[x : y + 1]))
    elif m := re.match(r"move position (?P<X>\d+) to position (?P<Y>\d+)", line):
        x, y = int(m.group("X")), int(m.group("Y"))
        rchar = psswrd[x]
        del psswrd[x]
        psswrd = psswrd[:y] + [rchar] + psswrd[y:]
    elif m := re.match(r"swap letter (?P<X>\w) with letter (?P<Y>\w)", line):
        x = psswrd.index(m.group("X"))
        y = psswrd.index(m.group("Y"))
        psswrd[x], psswrd[y] = psswrd[y], psswrd[x]
    elif m := re.match(r"rotate based on position of letter (?P<X>\w)", line):
        x = psswrd.index(m.group("X"))
        rot = x + 1
        if x >= 4:
            rot += 1
        rot %= len(psswrd)
        psswrd = psswrd[-rot:] + psswrd[:-rot]
    else:
        raise Exception(f"Unknown line: {line!r}")
    assert sorted(old) == sorted(psswrd)

print("".join(psswrd))

psswrd = list("fbgdceah")
rot_pos = [(2 * x + 1 + (1 if x >= 4 else 0)) % len(psswrd) for x in range(len(psswrd))]

for line in reversed(data):
    old = list(psswrd)
    if m := re.match(r"swap position (?P<X>\d+) with position (?P<Y>\d+)", line):
        x, y = int(m.group("X")), int(m.group("Y"))
        psswrd[x], psswrd[y] = psswrd[y], psswrd[x]
    elif m := re.match(r"rotate left (?P<X>\d+) steps?", line):
        x = int(m.group("X"))
        psswrd = psswrd[-x:] + psswrd[:-x]
    elif m := re.match(r"rotate right (?P<X>\d+) steps?", line):
        x = int(m.group("X"))
        psswrd = psswrd[x:] + psswrd[:x]
    elif m := re.match(r"reverse positions (?P<X>\d+) through (?P<Y>\d+)", line):
        x, y = int(m.group("X")), int(m.group("Y"))
        psswrd[x : y + 1] = list(reversed(psswrd[x : y + 1]))
    elif m := re.match(r"move position (?P<X>\d+) to position (?P<Y>\d+)", line):
        x, y = int(m.group("X")), int(m.group("Y"))
        rchar = psswrd[y]
        del psswrd[y]
        psswrd = psswrd[:x] + [rchar] + psswrd[x:]
    elif m := re.match(r"swap letter (?P<X>\w) with letter (?P<Y>\w)", line):
        x = psswrd.index(m.group("X"))
        y = psswrd.index(m.group("Y"))
        psswrd[x], psswrd[y] = psswrd[y], psswrd[x]
    elif m := re.match(r"rotate based on position of letter (?P<X>\w)", line):
        # need to reverse this
        x = psswrd.index(m.group("X"))
        oldpos = rot_pos.index(x)
        rot = oldpos - x
        rot %= len(psswrd)
        psswrd = psswrd[-rot:] + psswrd[:-rot]
    else:
        raise Exception(f"Unknown line: {line!r}")
    assert sorted(old) == sorted(psswrd)

print("".join(psswrd))
