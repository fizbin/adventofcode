import sys
import re

myinput = "aoc16.in"
if len(sys.argv) > 2:
    myinput = sys.argv[1]

with open(myinput, "r", encoding="utf-8") as inputf:
    data = inputf.read().strip().split(",")

move_re = re.compile(r"s(\d+)|x(\d+)/(\d+)|p(\w)/(\w)")
lineup = list("abcdefghijklmnop")
for move in data:
    m = move_re.match(move)
    if m.start(1) >= 0:
        sz = int(m.group(1))
        lineup = lineup[-sz:] + lineup[:-sz]
    elif m.start(2) >= 0:
        a = int(m.group(2))
        b = int(m.group(3))
        (lineup[a], lineup[b]) = (lineup[b], lineup[a])
    elif m.start(4) >= 0:
        a = lineup.index(m.group(4))
        b = lineup.index(m.group(5))
        (lineup[a], lineup[b]) = (lineup[b], lineup[a])

part_1_answer = "".join(lineup)
print(part_1_answer)


def mul_permutations(perm_a, perm_b):
    idx_a = [ord(x) - ord("a") for x in perm_a]
    return "".join(perm_b[x] for x in idx_a)


def pow_perm(perm, n):
    if n == 0:
        return "".join(sorted(perm))
    if n == 1:
        return perm
    sub_power = pow_perm(perm, n // 2)
    if n % 2 == 0:
        return mul_permutations(sub_power, sub_power)
    else:
        sub_power_2 = mul_permutations(sub_power, perm)
        return mul_permutations(sub_power, sub_power_2)


lineup1 = list("abcdefghijklmnop")
lineup2 = list("abcdefghijklmnop")
for move in data:
    m = move_re.match(move)
    if m.start(1) >= 0:
        sz = int(m.group(1))
        lineup1 = lineup1[-sz:] + lineup1[:-sz]
    elif m.start(2) >= 0:
        a = int(m.group(2))
        b = int(m.group(3))
        (lineup1[a], lineup1[b]) = (lineup1[b], lineup1[a])
    elif m.start(4) >= 0:
        a = lineup2.index(m.group(4))
        b = lineup2.index(m.group(5))
        (lineup2[a], lineup2[b]) = (lineup2[b], lineup2[a])

assert part_1_answer == mul_permutations("".join(lineup1), "".join(lineup2))

print(
    mul_permutations(
        pow_perm("".join(lineup1), 1000000000), pow_perm("".join(lineup2), 1000000000)
    )
)
