import sys
import re

def doit():
    with open('aoc16.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)

    rules = {}
    nearby = []
    state = 0
    myticket = None
    for line in data:
        if not line:
            state += 1
            continue
        if state == 0:
            m = re.match(r'([^:]+): (\d+)-(\d+) or (\d+)-(\d+)', line)
            rules[m.group(1)] = (int(m.group(2)), int(m.group(3)),
                                 int(m.group(4)), int(m.group(5)))
        elif state == 1:
            if line == 'your ticket:':
                continue
            myticket = [int(x) for x in line.split(',')]
        elif state == 2:
            if line == 'nearby tickets:':
                continue
            nearby.append([int(x) for x in line.split(',')])
    allvalid = set()
    for rule in rules.values():
        (a, b, c, d) = rule
        allvalid = allvalid | set(range(a, b+1)) | set(range(c, d+1))
    badsum = 0
    validn = []
    for ticket in nearby:
        isvalid = True
        for val in ticket:
            if val not in allvalid:
                isvalid = False
                badsum += val
        if isvalid:
            validn.append(ticket)
    print("badsum", badsum)

    assigned = {}  # fieldname -> position
    while len(assigned) < len(rules):
        # print(len(assigned), len(rules))
        madechange = False
        possible = [set(rules) - set(assigned) for _ in range(len(myticket))]
        for known, knownidx in assigned.items():
            possible[knownidx] = set([known])
        for ticket in validn:
            for idx, tickval in enumerate(ticket):
                newposs = set()
                for poss in possible[idx]:
                    (a, b, c, d) = rules[poss]
                    if (a <= tickval <= b) or (c <= tickval <= d):
                        newposs.add(poss)
                possible[idx] = newposs
        for idx, poss in enumerate(possible):
            if len(poss) == 1 and min(poss) not in assigned:
                assigned[min(poss)] = idx
                madechange = True
        assert madechange, "Discovered rules: " + repr(assigned)

    myprod = 1
    for fld, idx in assigned.items():
        if fld.startswith('departure '):
            myprod *= myticket[idx]
    print(myprod)

if __name__ == '__main__':
    doit()
