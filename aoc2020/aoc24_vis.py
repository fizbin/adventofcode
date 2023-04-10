import sys
import re
import time

def move(loc, instr):
    if instr == 'nw':
        loc += 1j
    if instr == 'ne':
        loc += 1+1j
    if instr == 'se':
        loc += -1j
    if instr == 'sw':
        loc += -1-1j
    if instr == 'e':
        loc += 1
    if instr == 'w':
        loc += -1
    return loc 

def get_all_nbhd(flipped):
    return set(move(loc, instr) for loc in flipped for instr in 'e w ne nw se sw'.split())

def get_nbr_count(loc, flipped):
    return len(flipped & get_all_nbhd([loc]))

def do_one_gen(flipped):
    flipped2 = set()
    for pot in (flipped | get_all_nbhd(flipped)):
        nc = get_nbr_count(pot, flipped)
        if (pot in flipped and nc in (1, 2)) or (pot not in flipped and nc == 2):
            flipped2.add(pot)
    return flipped2

uniblocks = {
    # 1 2
    # 4 8
    0: ' ',
    1: '\u2598',
    2: '\u259d',
    3: '\u2580',
    4: '\u2596',
    5: '\u258c',
    6: '\u259e',
    7: '\u259b',
    8: '\u2597',
    9: '\u259a',
    10: '\u2590',
    11: '\u259c',
    12: '\u2584',
    13: '\u2599',
    14: '\u259f',
    15: '\u2588'
    }
def gridprint(flipped, maximag, minimag, maxreal, minreal):
    # maximag = int(max(f.imag for f in flipped))
    # minimag = int(min(f.imag for f in flipped))
    # minreal = int(min(f.real for f in flipped))
    # maxreal = int(max(f.real for f in flipped))

    txt = ''
    indent = ''
    for myimag in range(maximag+1, minimag-3, -2):
        txt += indent
        for myreal in range(minreal, maxreal+1):
            point = (3*int((myreal + myimag*1j) in flipped)
                     + 4*int((myreal + myimag*1j - 1 - (1j)) in flipped)
                     + 8*int((myreal + myimag*1j - (1j)) in flipped))
            txt += uniblocks[point]
        indent += ' '
        txt += '\n'
    print(txt)


def doit():
    with open('aoc24.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = [x.strip() for x in f]

    flipped = set()
    for line in data:
        instructions = re.findall('ne|nw|sw|se|e|w', line)
        loc = 0+0j
        for instr in instructions:
            loc = move(loc, instr)
        if loc in flipped:
            flipped.remove(loc)
        else:
            flipped.add(loc)
    print(len(flipped))

    for day in range(200):
        print('\x1b[2J\x1b[H', end='')
        print(len(flipped))
        gridprint(flipped, 94, -144, 94, -94)
        flipped = do_one_gen(flipped)
        # viewport = (min(f.imag for f in flipped), max(f.imag for f in flipped),
        #             min(f.real for f in flipped), max(f.real for f in flipped))
        # print(viewport)
        time.sleep(0.1)
    time.sleep(5)


if __name__ == '__main__':
    doit()
