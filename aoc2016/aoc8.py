import sys
import re
import numpy as np


infile = "aoc8.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = [x.strip() for x in infilep]

display = np.zeros((6, 50), dtype=bool)


def show_display():
    print()
    print("\n".join("".join(list(r)) for r in np.where(display, "\u2588", ".")))


for line in data:
    m = re.match(r"rect (\d+)x(\d+)", line)
    if m:
        display[0 : int(m.group(2)), 0 : int(m.group(1))] = True
        continue
    m = re.match(r"rotate row y=(-?\d+) by (-?\d+)", line)
    if m:
        ind_r, ind_c = np.ogrid[:6, :50]
        adj = np.zeros_like(ind_r)
        adj[int(m.group(1))][0] = -(int(m.group(2)) % 50)
        display[...] = display[ind_r, ind_c + adj]
        continue
    m = re.match(r"rotate column x=(-?\d+) by (-?\d+)", line)
    if m:
        ind_r, ind_c = np.ogrid[:6, :50]
        adj = np.zeros_like(ind_c)
        adj[0][int(m.group(1))] = -(int(m.group(2)) % 6)
        display[...] = display[ind_r + adj, ind_c]
        continue
    raise Exception(f"bad line {line!r}")

print(sum(display.ravel().astype(int)))

show_display()
