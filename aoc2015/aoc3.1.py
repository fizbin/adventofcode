import sys

with open('aoc3.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read().strip()

spot = 0j
spots = set([spot])
for c in data:
    spot += {'^': 1j, 'v': -1j, '>': 1, '<': -1}[c]
    spots.add(spot)

print(len(spots))

