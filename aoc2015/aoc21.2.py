import itertools

player_hp = 100

boss_hp = 109
boss_damage = 8
boss_armor = 2

weapons = [(n, int(c), int(d), int(a))
           for (n, c, d, a) in [x.split() for x in '''
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0
'''.strip().splitlines()]]

armors = [(n, int(c), int(d), int(a))
         for (n, c, d, a) in [x.split() for x in '''
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5
'''.strip().splitlines()]]

rings = [(n, int(c), int(d), int(a))
         for (n, c, d, a) in [x.split() for x in '''
Damage+1    25     1       0
Damage+2    50     2       0
Damage+3   100     3       0
Defense+1   20     0       1
Defense+2   40     0       2
Defense+3   80     0       3
'''.strip().splitlines()]]

dblrings = [(n1+n2, c1+c2, d1+d2, a1+a2)
            for ((n1, c1, d1, a1), (n2, c2, d2, a2))
            in itertools.combinations(rings, 2)]

maxcost = 0
for armor in [('None', 0, 0, 0)] + armors:
#    for weapon in [('None', 0, 0, 0)] + weapons:
    for weapon in weapons:
        for ring in [('None', 0, 0, 0)] + rings + dblrings:
            aname = armor[0]
            wname = weapon[0]
            rname = ring[0]
            cost = armor[1] + weapon[1] + ring[1]
            to_boss = max(1, weapon[2] + ring[2] - boss_armor)
            to_player = max(1, - armor[3] - ring[3] + boss_damage)
            boss_hits = 1 + ((boss_hp - 1) // to_boss)
            p_hits = 1 + ((player_hp - 1) // to_player)
            if p_hits >= boss_hits:
                continue  # we win
            print("%s %s %s: %d" % (aname, wname, rname, cost))
            maxcost = max(cost, maxcost)

print(maxcost)
