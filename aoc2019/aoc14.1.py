import numpy as np
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

needs = {'FUEL': 1}

needschem = ['FUEL']
found_it = False
while needschem:
    for chem in needschem:
        while needs[chem] > 0:
            # print(needs)
            (quant, specs) = reaction_src[chem]
            for (src_chem, src_quant) in specs:
                needs[src_chem] = needs.get(src_chem, 0) + src_quant
            needs[chem] -= quant
    needschem = list(x for x in needs if needs[x] > 0 and x != 'ORE')

print(needs['ORE'])
