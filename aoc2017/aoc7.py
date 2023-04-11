import sys
import re

infile = "aoc7.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

progweights = {}
progchildren = {}
carried = set()
for line in data:
    m = re.match(r"(\w+) \((\d+)\) *(?:->([\w, ]+))?$", line.strip())
    progweights[m.group(1)] = int(m.group(2))
    if m.group(3) is not None:
        children = list(re.findall(r"\w+", m.group(3)))
        carried.update(children)
        progchildren[m.group(1)] = children

bottom = max(set(progweights) - carried)
print(bottom)


def find_recursive_weight(prog):
    return progweights[prog] + sum(
        find_recursive_weight(x) for x in progchildren.get(prog, [])
    )


def find_unbalanced(bot_prog):
    child_weights = [find_recursive_weight(chld) for chld in progchildren[bot_prog]]
    if len(set(child_weights)) <= 1:
        return None
    if len(set(child_weights)) > 2:
        raise Exception("Multiple bad weights")
    for g in sorted(set(child_weights)):
        if len([x for x in child_weights if x == g]) == 1:
            child = progchildren[bot_prog][child_weights.index(g)]
            ans = find_unbalanced(child)
            if ans is not None:
                return ans
            if len(child_weights) > 2:
                k = max(x for x in child_weights if x != g)
                return progweights[child] + k - g
    raise Exception("Should have found answer by now")


print(find_unbalanced(bottom))
