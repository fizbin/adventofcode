import re
import sys

with open("aoc19.in" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read()

(rules_src, start_molecule) = data.split('\n\n')
start_molecule = start_molecule.strip()
rules = re.findall(r'(\w+) => (\w+)', rules_src)

molecules = set()

for (from_str, to_str) in rules:
    for m in re.finditer(from_str, start_molecule):
        molecules.add(start_molecule[:m.start()] + to_str
                      + start_molecule[m.end():])

print(len(molecules))
