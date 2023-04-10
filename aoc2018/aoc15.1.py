import re
import sys
from heapq import heappush, heappop, heapify

data = '''
#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########
'''.strip()

datas = '''
#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######
'''.strip()

data = '''
#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
####### 
'''.strip()

data = '''
#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######
'''.strip()

data = '''
#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######
'''.strip()

datas = '''
#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######
'''.strip()

datas = '''
#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######
'''.strip()

data = '''
#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########
'''.strip()

data = '''
################################
######......###...##..##########
######....#G###G..##.G##########
#####...G##.##.........#########
##....##..#.##...........#######
#....#G.......##.........G.#####
##..##GG....G.................##
##.......G............#.......##
###.....G.....G#......E.......##
##......##....................##
#.....####......G.....#...######
#.#########.G....G....#E.#######
###########...#####......#######
###########..#######..E.......##
###########.#########......#.###
########..#.#########.........##
#######G....#########........###
##.##.#.....#########...EE#..#.#
#...GG......#########.#...##..E#
##...#.......#######..#...#....#
###.##........#####......##...##
###.........................#..#
####.............##........###.#
####............##.........#####
####..##....###.#...#.....######
########....###..............###
########..G...##.###...E...E.###
#########...G.##.###.E....E.####
#########...#.#######.......####
#############..########...######
##############.########.########
################################
'''.strip()

if len(sys.argv) > 1:
    data = open(sys.argv[1]).read().strip()

data = [x.strip() for x in data.splitlines()]

spaces = {}       #  (y, x) => '.' or '#'
combatants = []   #  (y, x, 'G' or 'E', hp)
for (y, ln) in enumerate(data):
    for (x, pic) in enumerate(ln):
        if pic in '.#':
            spaces[(y, x)] = pic
        elif pic in 'GE':
            spaces[(y, x)] = '.'
            combatants.append((y, x, pic, 200))


def enemy(typ):
    if typ == 'E':
        return 'G'
    return 'E'


def find_closest(from_loc, to_loc_set):
    visited = set()
    edgen = set([from_loc])
    foundset = set([])
    dist = 0
    if from_loc in to_loc_set:
        return edgen
    while edgen and not foundset:
        dist += 1
        edge = edgen
        edgen = set([])
        for (y, x) in edge:
            neighbors = [(y-1, x), (y, x-1), (y, x+1), (y+1, x)]
            for n in neighbors:
                if n in to_loc_set:
                    foundset.add(n)
                elif spaces.get(n, '#') == '.' and n not in visited:
                    if not [c for c in filter(None, combatants)
                            if (c[0], c[1]) == n]:
                        edgen.add(n)
                visited.add(n)
    return foundset


def attack(target):
    for (n, c) in list(enumerate(combatants)):
        if c:
            if (c[0], c[1]) == target:
                if c[2] == 'G':
                    hp = c[3] - 3
                else:
                    hp = c[3] - 3
                if hp <= 0:
                    if c[2] == 'E':
                        pass
                        # raise Exception("elf died")
                    combatants[n] = None
                else:
                    combatants[n] = (c[0], c[1], c[2], hp)
                return
    raise Exception("Inconsistency: attacking %s but combatants are %r" %
                    (target, combatants))


roundnum = 0
finished = False
while not finished:
    for (_, comb_no) in list(sorted((c, n)
                                    for (n, c)
                                    in enumerate(combatants)
                                    if c is not None)):
        combatant = combatants[comb_no]
        if combatants[comb_no] is None:
            continue
        (y, x, typ, hp) = combatants[comb_no]
        neighbors = [(y-1, x), (y, x-1), (y, x+1), (y+1, x)]
        enemies = [c for c in filter(None, combatants)
                   if c[2] != typ]
        if not enemies:
            finished = True
            break
        enemy_locs = [(ey, ex) for (ey, ex, _1, _2) in enemies]
        comb_locs = [(cy, cx) for (cy, cx, _1, _2) in filter(None, combatants)]
        near_enemies = [c for c in enemies
                        if (c[0], c[1]) in neighbors]
        if not near_enemies:
            enemy_neighbors = [(ey+ya, ex+xa)
                               for (ey, ex) in enemy_locs
                               for (ya, xa) in [(-1, 0), (1, 0),
                                                (0, -1), (0, 1)]
                               if (((ey+ya, ex+xa) not in comb_locs)
                                   and spaces.get((ey+ya, ex+xa), '#') == '.')]
            nearest_targets = find_closest((y, x),
                                           set(enemy_neighbors))
            if not nearest_targets:
                continue
            target = min(nearest_targets)
            move_p = [n for n in neighbors if ((n not in comb_locs) and
                                               (spaces.get(n, '#') == '.'))]
            move_t = min(find_closest(target, move_p))
            combatants[comb_no] = (move_t[0], move_t[1], typ, hp)
            (y, x) = move_t
            neighbors = [(y-1, x), (y, x-1), (y, x+1), (y+1, x)]
            near_enemies = [c for c in enemies
                            if (c[0], c[1]) in neighbors]
        if near_enemies:
            print("%s (now at %s) is considering %s" %
                  (combatant, (y, x), near_enemies))
            min_hp = min(c[3] for c in near_enemies)
            target = min(c for c in near_enemies if c[3] == min_hp)
            print("%s (now at %s) is attacking %s" %
                  (combatant, (y, x), target))
            attack((target[0], target[1]))

    print(roundnum + 1)
    if finished:
        print("Interrupted")
    print(list(sorted(filter(None, combatants))))

    if not finished:
        roundnum += 1



print(list(filter(None, combatants)))
print(roundnum)
print(sum(c[3] for c in filter(None, combatants)))

print('')
print((roundnum) * sum(c[3] for c in filter(None, combatants)))
