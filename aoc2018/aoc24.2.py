from __future__ import print_function

import sys
import re
import io

with open('aoc24.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

def run_with_boost(boost, file=sys.stdout):
    immune_system = []
    infection = []

    mode = None
    for line in data:
        if not line.strip():
            continue
        if line.startswith('Immune System:'):
            mode = 'M'
            group_num = 0
        elif line.startswith('Infection:'):
            mode = 'N'
            group_num = 0
        else:
            m = re.match(r'(\d+) units each with (\d+) hit points *'
                         r'(?:(\([^()]*\)))?'
                         r' *with an attack that does (\d+) (\S+) damage'
                         r' at initiative (\d+)', line)
            (n_units, unit_hp, special_text, damage_hp, damage_type,
             initiative) = m.groups()
            group_num += 1
            weaks = set()
            immunes = set()
            if special_text is not None:
                m = re.search(r'immune to (\S[^;\)]*)', special_text)
                if m:
                    immunes = set(t.strip() for t in m.group(1).split(','))
                m = re.search(r'weak to (\S[^;\)]*)', special_text)
                if m:
                    weaks = set(t.strip() for t in m.group(1).split(','))
            unit = {'n_units': int(n_units),
                    'unit_hp': int(unit_hp),
                    'damage_hp': int(damage_hp) + (boost if mode == 'M' else 0),
                    'damage_type': damage_type,
                    'weaks': weaks,
                    'immunes': immunes,
                    'initiative': int(initiative),
                    'team': mode,
                    'group_no': group_num}
            {'M': immune_system, 'N': infection}[mode].append(unit)


    print('immune_system', immune_system, file=file)
    print('infection', infection, file=file)

    def damage_by(from_group, to_group):
        base = from_group['n_units'] * from_group['damage_hp']
        if from_group['damage_type'] in to_group['weaks']:
            base *= 2
        if from_group['damage_type'] in to_group['immunes']:
            base = 0
        return base


    while immune_system and infection:
        # target selection
        stalemate = True
        groups = sorted(immune_system + infection,
                        key=lambda u: (-u['n_units']*u['damage_hp'],
                                       -u['initiative']))
        targets = {}
        for group in groups:
            enemy_groups = filter(lambda u: u['team'] != group['team'], groups)
            target_possibles = sorted(
                filter(lambda u: (u not in targets.values()
                                  and damage_by(group, u) > 0),
                       enemy_groups),
                key=lambda u: (-damage_by(group, u), -u['n_units']*u['damage_hp'],
                               -u['initiative'])
            )
            if target_possibles:
                print(f"{group['team']} group {group['group_no']} targets "
                      f"other team {target_possibles[0]['group_no']} ",
                      file=file)
                targets[(group['team'], group['group_no'])] = target_possibles[0]
        groups.sort(key=lambda u: -u['initiative'])
        for group in groups:
            if group['n_units'] <= 0:
                continue
            target = targets.get((group['team'], group['group_no']))
            if target is None:
                continue
            damage = damage_by(group, target)
            target_units_lost = damage // target['unit_hp']
            if target_units_lost > 0:
                stalemate = False
            target['n_units'] = max(0, target['n_units'] - target_units_lost)
            print(f"{group['team']} group {group['group_no']} attacks "
                  f"{target['team']} group {target['group_no']} killing "
                  f"{target_units_lost}", file=file)
        immune_system = list(filter(lambda u: u['n_units'] > 0, immune_system))
        infection = list(filter(lambda u: u['n_units'] > 0, infection))

        print('--\nImmune System:', file=file)
        for group in sorted(immune_system, key=lambda u: u['group_no']):
            print(f"Group {group['group_no']} contains {group['n_units']} units",
                  file=file)
        print('Infection:', file=file)
        for group in sorted(infection, key=lambda u: u['group_no']):
            print(f"Group {group['group_no']} contains {group['n_units']} units",
                  file=file)
        if stalemate:
            print("Stalemate!")
            return False

    print('immune_system', immune_system, file=file)
    print('infection', infection, file=file)

    print('total units', sum(x['n_units'] for x in (immune_system + infection)),
          file=file)

    return bool(immune_system)

low = 0
high = 2048

while not run_with_boost(high, file=io.StringIO()):
    print("Trying high", high)
    high *= 2

while high - low > 1:
    mid = (low + high) // 2
    print("Trying mid", mid)
    if run_with_boost(mid, file=io.StringIO()):
        high = mid
    else:
        low = mid

run_with_boost(high)
print("Minimal boost", high)
