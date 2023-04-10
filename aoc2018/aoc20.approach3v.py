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
palette[3*3+1] = 240
palette[3*3+2] = 0
palette[4*3] = 224
palette[4*3+1] = 224
palette[4*3+2] = 0
palette[5*3] = 150
palette[5*3+1] = 150
palette[5*3+2] = 150
image.putpalette(palette)
drawer = ImageDraw.Draw(image)
font = ImageFont.truetype('RobotoMono-Light.ttf', 15)
offset = 120+120j


def pt1(z):
    z += offset
    (x, y) = (z.real, z.imag)
    drawer.rectangle([(2*x, 2*y), (2*x+1, 2*y+1)], 1)


def pt2(z):
    z += offset
    (x, y) = (z.real, z.imag)
    drawer.rectangle([(2*x, 2*y), (2*x+1, 2*y+1)], 2)


def pt3(z):
    z += offset
    (x, y) = (z.real, z.imag)
    drawer.rectangle([(2*x, 2*y), (2*x+1, 2*y+1)], 3)


def pt4(z):
    z += offset
    (x, y) = (z.real, z.imag)
    drawer.rectangle([(2*x, 2*y), (2*x+1, 2*y+1)], 4)


pt2(0j)
images = [image.copy()]
pt1(0j)


def findmatching(curr_indx, target, directions):
    i = curr_indx + 1
    while i < len(directions):
        now = directions[i]
        if now == target:
            return i
        if now == '(':
            i = findmatching(i, ')', directions) + 1
        elif now == ')':
            return None
        else:
            i += 1
    else:
        raise Exception("findmatching(%r, %r)" % (curr_indx, target))


State = collections.namedtuple('State', ['loc', 'parsing', 'finish'])
state = State(loc=0j, parsing=0, finish=())
states = set([state])
waiting = set()
done = set()
while states | waiting:
#    print("states", states)
#    print("waiting", waiting)
    if not states:
        lowest_parse = min(s.parsing for s in waiting)
        states = set(s for s in waiting if s.parsing == lowest_parse)
        waiting = waiting - states
    nstates = set()
    maxparsing = 0
    minparsing = len(global_directions)
    for state in states:
#        print("Now handling", state)
        if state.parsing >= len(global_directions):
            done.add(state)
            continue
        minparsing = min(minparsing, state.parsing)
        maxparsing = max(maxparsing, state.parsing)
        now = global_directions[state.parsing]
        if now in 'NSEW':
            move = dirs[now]
            new_locset = set([])
            nloc = state.loc + move
            graph[state.loc].add(nloc)
            graph[nloc].add(state.loc)
            pt1(2*state.loc + move)
            pt1(2*nloc)
            nstates.add(state._replace(loc=nloc, parsing=state.parsing+1))
        elif now in '|)':
            if state.finish:
                basically_done = (
                    any(f >= len(global_directions) for f in state.finish)
                    and
                    all(global_directions[f] in '|)' for f in state.finish
                        if f < len(global_directions)))
                nstate = state._replace(parsing=state.finish[0],
                                        finish=state.finish[1:])
                if basically_done:
                    done.add(nstate)
                else:
                    waiting.add(nstate)
        elif now == '(':
            subs = [state.parsing+1]
            nfinish = (
                (1+findmatching(state.parsing, ')', global_directions),)
                + state.finish)
            nextbar = findmatching(state.parsing, '|', global_directions)
            while nextbar is not None:
                subs.append(nextbar+1)
                nextbar = findmatching(nextbar, '|', global_directions)
            for sub in subs:
                nstates.add(State(loc=state.loc, parsing=sub, finish=nfinish))
    states = nstates
    for loc in set(s.loc for s in done):
        pt3(2*loc)
    for loc in set(s.loc for s in waiting):
        pt4(2*loc)
    for loc in set(s.loc for s in states):
        pt2(2*loc)
    msg = "parse range (%d-%d) %d running %d waiting %d done" % (
        minparsing, maxparsing,
        len(states), len(waiting), len(done))
    sizing = drawer.textsize(msg, font)
    drawer.text((475 - sizing[0], 475 - sizing[1]), msg, font=font, fill=4)
    images.append(image.copy())
    drawer.rectangle([(0, 460), (479, 479)], fill=0)
    for loc in set(s.loc for s in (done | states | waiting)):
        pt1(2*loc)


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
