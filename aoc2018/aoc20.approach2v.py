from __future__ import print_function

import sys
import collections
from PIL import Image, ImageFont, ImageDraw

with open('aoc20.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read().strip()

if data[0] != '^':
    raise Exception("Didn't start with ^")
if data[-1] != '$':
    raise Exception("Didn't end with $")

global_directions = data[1:-1]

dirs = {'N': 0-1j, 'S': 0+1j, 'E': 1+0j, 'W': -1+0j}
graph = collections.defaultdict(set)
font = ImageFont.load_default()
image = Image.new('P', (480, 480))
palette = image.getpalette()
palette[1*3] = 200
palette[1*3+1] = 0
palette[1*3+2] = 0
palette[2*3] = 255
palette[2*3+1] = 255
palette[2*3+2] = 255
palette[3*3] = 0
palette[3*3+1] = 200
palette[3*3+2] = 0
palette[4*3] = 150
palette[4*3+1] = 150
palette[4*3+2] = 150
image.putpalette(palette)
drawer = ImageDraw.Draw(image)
font = ImageFont.truetype('RobotoMono-Light.ttf', 15)


def pt1(x, y):
    drawer.rectangle([(2*x, 2*y), (2*x+1, 2*y+1)], 1)


def pt2(x, y):
    drawer.rectangle([(2*x, 2*y), (2*x+1, 2*y+1)], 2)


def pt3(x, y):
    drawer.rectangle([(2*x, 2*y), (2*x+1, 2*y+1)], 3)


pt2(120, 120)
images = [image.copy()]
pt1(120, 120)


def parse(startset, stuff, waits=frozenset(), level=0):
    locset = set(startset)
    tot_locset = set()
    new_locs = set()
    while stuff:
        now = stuff.pop(0)
        if now in '(|)':
            for loc in new_locs:
                pt2(int((2*loc+120).real), int((2*loc+120j).imag))
            for loc in waits:
                pt3(int((2*loc + 120).real), int((2*loc + 120j).imag))
            msg = "'%s' with %d remaining; nesting level %d" % (now, len(stuff), level)
            sizing = drawer.textsize(msg, font)
            drawer.text((475 - sizing[0], 475 - sizing[1]), msg, font=font, fill=4)
            images.append(image.copy())
            drawer.rectangle([(0, 460), (479, 479)], fill=0)
            for loc in (new_locs | waits):
                pt1(int((2*loc + 120).real), int((2*loc + 120j).imag))
            new_locs = set()
        if now == '(':
            locset = parse(locset, stuff, waits=frozenset(locset) | waits,
                           level=level + 1)
        elif now == '|':
            tot_locset.update(locset)
            locset = set(startset)
        elif now == ')':
            tot_locset.update(locset)
            return tot_locset
        elif now in 'NSEW':
            move = dirs[now]
            new_locset = set([])
            for loc in locset:
                nloc = loc + move
                graph[loc].add(nloc)
                graph[nloc].add(loc)
                new_locset.add(nloc)
                pt1(int((2*loc+move+120).real), int((2*loc+move+120j).imag))
                pt1(int((2*nloc+120).real), int((2*nloc+120j).imag))
            new_locs.update(new_locset)
            locset = new_locset
    for loc in new_locs:
        pt2(int((2*loc+120).real), int((2*loc+120j).imag))
    for loc in waits:
        pt3(int((2*loc + 120).real), int((2*loc + 120j).imag))
    msg = "%d remaining; nesting level %d" % (len(stuff), level)
    sizing = drawer.textsize(msg, font)
    drawer.text((475 - sizing[0], 475 - sizing[1]), msg, font=font, fill=4)
    images.append(image.copy())
    drawer.rectangle([(0, 460), (479, 479)], fill=0)
    for loc in (new_locs | waits):
        pt1(int((2*loc + 120).real), int((2*loc + 120j).imag))
    new_locs = set()


try:
    parse(set([0+0j]), list(global_directions))
except IndexError:
    pass

# msg = "%d remaining; nesting level %d" % (0, 0)
# sizing = drawer.textsize(msg, font)
# drawer.text((475 - sizing[0], 475 - sizing[1]), msg, font=font, fill=4)
# images.append(image)

dists = {0+0j: 0}
working = collections.deque([0+0j])
while working:
    now = working.popleft()
    for nloc in graph[now]:
        if nloc not in dists or dists[nloc] > dists[now] + 1:
            working.append(nloc)
            dists[nloc] = dists[now] + 1


for n in range(len(images)):
    images[n].save('aoc20img/image%06d.gif' % (n,), optimize=True)

print(max(dists.values()))

print(len([x for (x, d) in dists.items() if d >= 1000]))

print(max(z.real for z in dists),
      max(z.imag for z in dists),
      min(z.real for z in dists),
      min(z.imag for z in dists))
print(len(images))
# W(N|S)E
# ((0, 0), 0) []
#    W
# ((-1, 0), 1) []
#    (
# ((-1, 0), 2) [((-1, 0), 2)]
#    N
# ((-1, 1), 3) [((-1, 0), 2)]
#    |
#
