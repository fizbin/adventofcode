import sys
import re
import itertools

with open('aoc8.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = [re.findall(r'[a-z]+', x) for x in re.findall(r'[a-z ]+\|[a-z ]+', f.read())]
    # data = re.findall(r'\S+', f.read())
    # data = re.findall(r'(\d+),(\d+) -> (\d+),(\d+)', f.read())
    # data = [tuple(int(y) for y in tup) for tup in data]

output = [x[-4:] for x in data]
alloutputs = sum(output, [])
goods = [f for f in alloutputs if len(f) in [2, 4, 3, 7]]
print(len(goods))

on_vals = {
    0: 'abcefg',
    1: 'cf',
    2: 'acdeg',
    3: 'acdfg',
    4: 'bcdf',
    5: 'abdfg',
    6: 'abdefg',
    7: 'acf',
    8: 'abcdefg',
    9: 'abcdfg',
}

print()
total = 0
for puzzle in data:
    ins = [''.join(sorted(s)) for s in puzzle[:10]]
    outs = [''.join(sorted(s)) for s in puzzle[-4:]]
    known = {}
    rknown = {}
    for oneval in ins:
        if len(oneval) == 2:
            known[oneval] = 1
            rknown[1] = oneval
        if len(oneval) == 3:
            known[oneval] = 7
            rknown[7] = oneval
        elif len(oneval) == 4:
            known[oneval] = 4
            rknown[4] = oneval
        elif len(oneval) == 7:
            known[oneval] = 8
            rknown[8] = oneval
    for oneval in ins:
        if len(oneval) == 6 and not all(x in oneval for x in rknown[1]):
            known[oneval] = 6
            rknown[6] = oneval
        if len(oneval) == 5 and all(x in oneval for x in rknown[1]):
            known[oneval] = 3
            rknown[3] = oneval
    for oneval in ins:
        if len(oneval) == 6 and all(x in oneval for x in rknown[3]):
            known[oneval] = 9
            rknown[9] = oneval
    for oneval in ins:
        if oneval not in known:
            if len(oneval) == 5 and all(x in rknown[9] for x in oneval):
                known[oneval] = 5
                rknown[5] = oneval
    for oneval in ins:
        if oneval not in known:
            if len(oneval) == 6:
                known[oneval] = 0
                rknown[0] = oneval
            elif len(oneval) == 5:
                known[oneval] = 2
                rknown[2] = oneval
            else:
                raise Exception(f"Fuuu.... {known} and {rknown} and {oneval}")
    ans = ''.join(str(known[x]) for x in outs)
    total += int(ans)

print(total)