import sys
import re

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

def gridprint(flipped):
    maximag = int(max(f.imag for f in flipped))
    minimag = int(min(f.imag for f in flipped))
    minreal = int(min(f.real for f in flipped))
    maxreal = int(max(f.real for f in flipped))

    indent = ''
    for myimag in range(maximag+1, minimag-2, -1):
        print(indent, end='')
        for myreal in range(minreal, maxreal+1):
            borders = ('[', ']')
            if myreal == 0 and myimag == 0:
                borders = ('<', '>')
            if (myreal + myimag*1j) in flipped:
                print(f'{borders[0]}**{borders[1]}', end='')
            else:
                print(f'{borders[0]}  {borders[1]}', end='')
        indent += '  '
        print()


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
    print(flipped)
    print(len(flipped))
    for day in range(100):
        #gridprint(flipped)
        flipped = do_one_gen(flipped)
    print(len(flipped))


if __name__ == '__main__':
    doit()
