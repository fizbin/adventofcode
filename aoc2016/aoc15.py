import sys
import re

infile = "aoc15.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)


def extended_euclid(a, b):
    # returns (gcd, amul, bmul) such that gcd = a*amul + b*bmul
    # assumes that a and b are > 0
    if a < b:
        (g, bm, am) = extended_euclid(b, a)
        return (g, am, bm)
    if a % b == 0:
        return (b, 0, 1)
    c = a % b
    (g, bm, cm) = extended_euclid(b, c)
    # okay, now:
    #  c == a - (a // b)*b   AND
    #  g = bm*b + cm*c
    #    = bm*b + cm*a - (a // b)*cm*b
    #    = cm*a + (bm - (a//b)*cm)*b
    return (g, cm, (bm - (a // b) * cm))


def solve_gcd(ares, amod, bres, bmod):
    # returns answer x such that x % amod == ares and x % bmoc == bres
    (g, am, bm) = extended_euclid(amod, bmod)
    assert g == 1
    ans1 = ares * bm * bmod + bres * am * amod
    return ans1 % (amod * bmod)


disks = []
for line in data:
    #     Disc #1 has 13 positions; at time=0, it is at position 10.
    m = re.match(
        r"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)\.", line
    )
    disks.append((int(m.group(1)), int(m.group(2)), int(m.group(3))))

ans = 0
ansmod = 1
for discnum, discmod, discoff in disks:
    # want (discnum + discoff + ans) % discmod = 0
    target = (-discnum - discoff) % discmod
    ans = solve_gcd(target, discmod, ans, ansmod)
    ansmod *= discmod

print(ans)

target = (-len(disks) - 1) % 11
ans = solve_gcd(target, 11, ans, ansmod)
ansmod *= 11

print(ans)
