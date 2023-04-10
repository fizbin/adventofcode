import sys
import re
import copy

def find_oners(foods, wi, wa, known_i, known_a, startidx):
    while startidx < len(foods):
        (i1, a1) = foods[startidx]
        combi = (i1 & wi) - known_i
        comba = (a1 & wa) - known_a
        if len(combi) == 1 == len(comba):
            yield (combi, comba)
        elif combi and comba:
            yield from find_oners(foods, combi, comba, known_i, known_a, startidx + 1)
        startidx += 1

def doit():
    with open('aoc21.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)
    foods = []
    all_i = set()
    all_a = set()
    for line in data:
        m = re.match(r'(.*) \(contains (.*)\)', line)
        ingreds = set(m.group(1).split())
        all_i |= ingreds
        allergens = set(m.group(2).split(', '))
        all_a |= allergens
        foods.append((ingreds, allergens))
    known_mappings = {}
    known_a = set()
    known_i = set()
    changing = True
    while changing:
        changing = False
        for (i1, a1) in find_oners(foods, all_i, all_a, known_i, known_a, 0):
            print("found", (i1, a1))
            known_mappings[min(i1)] = min(a1)
            changing = True
            known_i.add(min(i1))
            known_a.add(min(a1))
    assert known_a == all_a, f"{known_a} known but all is {all_a}"
    total = 0
    for food in foods:
        total += len(food[0] - known_i)
    print(total)
    ingred = sorted(known_mappings, key=lambda f: known_mappings[f])
    print(','.join(ingred))

if __name__ == '__main__':
    doit()
