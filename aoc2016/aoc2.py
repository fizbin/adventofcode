import sys

infile = "aoc2.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

locx, locy = 1, 1
retval = ""
for line in data:
    for char in line:
        (xd, yd) = {"U": (-1, 0), "D": (1, 0), "L": (0, -1), "R": (0, 1)}.get(
            char, (0, 0)
        )
        locx += xd
        locy += yd
        locx = max(min(locx, 2), 0)
        locy = max(min(locy, 2), 0)
    retval += str(3 * locx + locy + 1)
print(retval)

# part 2

keypad = """
*******
***1***
**234**
*56789*
**ABC**
***D***
*******
""".strip().split(
    "\n"
)
locx, locy = 3, 3
retval = ""
for line in data:
    for char in line:
        (xd, yd) = {"U": (-1, 0), "D": (1, 0), "L": (0, -1), "R": (0, 1)}.get(
            char, (0, 0)
        )
        locx += xd
        locy += yd
        if keypad[locx][locy] == "*":
            locx -= xd
            locy -= yd
    retval += keypad[locx][locy]
print(retval)
