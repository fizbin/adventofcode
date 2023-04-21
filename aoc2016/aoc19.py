import sys

nelves = 3018458
if len(sys.argv) > 1:
    nelves = int(sys.argv[1])

next_elf = [0] + list(range(2, nelves + 1)) + [1]

cp = 1
nleft = nelves
while nleft > 1:
    rnext = next_elf[next_elf[cp]]
    next_elf[next_elf[cp]] = 0
    next_elf[cp] = rnext
    nleft -= 1
    cp = rnext

print(cp)

next_elf = [0] + list(range(2, nelves + 1)) + [1]

cp = nelves // 2  # cp is one before elf to be eliminated
nleft = nelves
while nleft > 1:
    rnext = next_elf[next_elf[cp]]
    next_elf[next_elf[cp]] = 0
    next_elf[cp] = rnext
    nleft -= 1
    if nleft % 2 == 0:
        cp = rnext

print(cp)
