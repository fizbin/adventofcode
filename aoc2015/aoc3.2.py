import sys

with open('aoc3.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read().strip()

spot1 = 0j
spot2 = 0j
spots = set([spot1])
flip = True
for c in data:
    direc = {'^': 1j, 'v': -1j, '>': 1, '<': -1}[c]
    if flip:
        spot1 += direc
    else:
        spot2 += direc
    flip = not flip
    spots.add(spot1)
    spots.add(spot2)

print(len(spots))
