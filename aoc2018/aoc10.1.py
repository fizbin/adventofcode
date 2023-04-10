import re
import numpy as np

with open('aoc10.in.txt') as f:
    datalines = list(f)

spots = []
velocities = []
for data in datalines:
    m = re.match(
        r'position=< *(-?\d+), *(-?\d+)> *velocity=< *(-?\d+), *(-?\d+)>',
        data)
    spots.append(list(m.group(1, 2)))
    velocities.append(list(m.group(3, 4)))

spotsnp = np.array(spots, dtype=int)
velocitiesnp = np.array(velocities, dtype=int)

i=0
while ((spotsnp[:, 1].max() - spotsnp[:, 1].min() > 200)
       and (spotsnp[:, 0].max() - spotsnp[:, 0].min() > 200)):
    spotsnp += velocitiesnp
    i += 1

while ((spotsnp[:, 1].max() - spotsnp[:, 1].min() < 210) or
       (spotsnp[:, 0].max() - spotsnp[:, 0].min() < 210)):
    spotsnp += velocitiesnp
    i += 1
    if (spotsnp.max(axis=0) - spotsnp.min(axis=0)).min() < 100:
        shifted = spotsnp - spotsnp.min(axis=0).reshape((1, 2))
        chararray = list([' '] * (1+shifted[:, 0].max())
                         for _ in range(1+shifted[:, 1].max()))
        for (x, y) in shifted.tolist():
            chararray[y][x] = '#'
        print('')
        print('=' * 80 + '   %d' % i)
        for ln in chararray:
            print(''.join(ln))
