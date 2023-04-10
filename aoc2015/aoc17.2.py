import sys

with open("aoc17.in.txt" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

containers = [int(x) for x in data]

work = [(0, 0, 0)]
combos = 0
comboways = {}
while work:
    (sofar, next_container, c_count) = work.pop()
    if sofar == 150:
        combos += 1
        comboways[c_count] = comboways.get(c_count, 0) + 1
        continue
    elif sofar > 150:
        continue
    if next_container >= len(containers):
        continue
    work.append((sofar + containers[next_container],
                 next_container + 1, c_count + 1))
    work.append((sofar, next_container + 1, c_count))

print(combos)
print(min(comboways.items()))
