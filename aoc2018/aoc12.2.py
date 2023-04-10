import numpy as np
import re


initial = ('##..##....#.#.####........##.#.#####.##..#.#..#.#...##.#####.###.##...#....##....#..###.#...#.#.#.#')

rules = {
    '##.#.': '.',
    '##.##': '.',
    '#..##': '.',
    '#.#.#': '.',
    '..#..': '#',
    '#.##.': '.',
    '##...': '#',
    '.#..#': '.',
    '#.###': '.',
    '.....': '.',
    '...#.': '#',
    '#..#.': '#',
    '###..': '#',
    '.#...': '#',
    '###.#': '#',
    '####.': '.',
    '.##.#': '#',
    '#.#..': '#',
    '.###.': '#',
    '.#.##': '.',
    '#####': '#',
    '....#': '.',
    '.####': '.',
    '.##..': '#',
    '##..#': '.',
    '#...#': '.',
    '..###': '#',
    '...##': '.',
    '#....': '.',
    '..##.': '.',
    '.#.#.': '#',
    '..#.#': '#',
}

gencount = 0
offset = 0
x = initial

while gencount < 200:
    if not re.match(r'^\.{10}', x):
        x = '.' * 10 + x
        offset -= 10
    x = re.sub(r'#\.{,9}$', '#..........', x)
    x2 = ['.'] * (0 + len(x))
    for ind in range(2, len(x)-2):
        x2[ind] = rules[''.join(x[ind-2:ind+3])]
    x = ''.join(x2)
    gencount += 1
    #print(x)
    tot = 0
    for (ind, val) in enumerate(x):
        if val == '#':
            tot += ind + offset
    print((gencount, tot))


#print(tot)
