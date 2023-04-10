import sys
import re

with open("aoc16.in.txt" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

evidence = '''
children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
'''

evidence = evidence.strip()
evidence_d = {}
for (thing, amt) in re.findall(r'(\w+): (\d+)', evidence):
    def gt(a):
        return (lambda t: int(t) > int(a))

    def lt(a):
        return (lambda t: int(t) < int(a))

    def eq(a):
        return (lambda t: t == a)

    if thing in ('cats', 'trees'):
        evidence_d[thing] = gt(amt)
    elif thing in ('pomeranians', 'goldfish'):
        evidence_d[thing] = lt(amt)
    else:
        evidence_d[thing] = eq(amt)

for ln in data:
    m = re.match(r'Sue (\d+): (.*)', ln)
    (sue_n, rest) = m.groups()
    could_be = True
    for (thing, amt) in re.findall(r'(\w+): (\d+)', rest):
        could_be = could_be and evidence_d[thing](amt)
    if could_be:
        print(f"Could be Sue {sue_n}")
