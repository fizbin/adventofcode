import re
import sys

with open('aoc14.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

reaction_src = {}
for line in data:
    m1 = re.match(r'(\S.*?) *=> *(\d+) *(\w+)', line)
    (lft, rgtq, rgts) = (m1.group(1), int(m1.group(2)), m1.group(3))
    specs = []
    for chemspec in re.split(r', *', lft):
        m2 = re.match(r'(\d+) *(\w+)', chemspec)
        specs.append((m2.group(2), int(m2.group(1))))
    reaction_src[rgts] = (rgtq, specs)


def find_needs_for(fuel_amount):
    needs = {'FUEL': fuel_amount}

    needschem = ['FUEL']
    while needschem:
        for chem in needschem:
            (quant, specs) = reaction_src[chem]
            mult = (needs[chem] + quant - 1) // quant
            for (src_chem, src_quant) in specs:
                needs[src_chem] = needs.get(src_chem, 0) + mult*src_quant
            needs[chem] -= mult*quant
        needschem = [x for x in needs if needs[x] > 0 and x != 'ORE']

    return needs['ORE']


low = 1
high = 10
while find_needs_for(high) <= 1000000000000:
    high *= 10

while high - low > 1:
    mid = (high + low) // 2
    if find_needs_for(mid) <= 1000000000000:
        low = mid
    else:
        high = mid

print(low)
