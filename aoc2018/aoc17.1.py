import re
import sys

with open('aoc17.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

exploded = []
for ln in data:
    m = re.match(r'x=(-?\d+), y=(-?\d+)\.\.(-?\d+)', ln)
    if m:
        exploded.extend((int(m.group(1)), y)
                        for y in range(int(m.group(2)), 1+int(m.group(3))))
        continue
    m = re.match(r'y=(-?\d+), x=(-?\d+)\.\.(-?\d+)', ln)
    if m:
        exploded.extend((x, int(m.group(1)))
                        for x in range(int(m.group(2)), 1+int(m.group(3))))
        continue
    raise Exception("Bad data %r" % (ln,))

miny = min(y for (x, y) in exploded)
maxy = max(y for (x, y) in exploded)

minx = min(x for (x, y) in exploded)
maxx = max(x for (x, y) in exploded)

print((miny, maxy))

ground = [['.'] * (3 + maxx) for _ in range(maxy + 3)]

for (x, y) in exploded:
    ground[y][x] = '#'

ground[miny - 1][500] = '|'

minChangeRow = miny
maxChangeRow = miny+2
while minChangeRow < maxy + 5:
    ystart = minChangeRow-1
    ystop = maxChangeRow
    maxChangeRow = 0
    minChangeRow = maxy + 10
    for y in range(ystart, maxy+2):
        if (y > ystop):
            break
        row = ground[y]
        downrow = ground[y+1]
        rowstr = ''.join(row)
        for m in re.finditer('[|]+', rowstr):
            left = row[m.start()-1]
            right = row[m.end()]
            below = downrow[m.start():m.end()]
            if left == '.' and below[0] in '#~':
                minChangeRow = min(minChangeRow, y)
                maxChangeRow = max(maxChangeRow, y)
                row[m.start()-1] = '|'
            if right == '.' and below[-1] in '#~':
                minChangeRow = min(minChangeRow, y)
                maxChangeRow = max(maxChangeRow, y)
                row[m.end()] = '|'
            if re.match('^[#~]*$', ''.join(below)):
                if (left == '#' and right == '#'):
                    minChangeRow = min(minChangeRow, y)
                    maxChangeRow = max(maxChangeRow, y)
                    row[m.start():m.end()] = ['~'] * (m.end() - m.start())
            elif '.' in below:
                minChangeRow = min(minChangeRow, y+1)
                maxChangeRow = max(maxChangeRow, y+1)
                ystop = max(ystop, y+1)
                for (off, v) in enumerate(below):
                    if v == '.':
                        downrow[m.start() + off] = '|'

    if False:  # enable for debugging
        print('')
        print(((minx, ystart), (maxx, ystop)))
        for y in range(ystart-1, ystop+1):
            print(''.join(ground[y][minx-1:maxx+2]))

count = 0
pt2count = 0
for y in range(miny, maxy+1):
    for v in ground[y]:
        if v in '~|':
            count += 1
        if v == '~':
            pt2count += 1

print('')
print(count)
print(pt2count)
