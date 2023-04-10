import sys
import re

# Day 22, part 2 only.

with open('aoc22.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)


# Note that deal_into_new, cut, and deal_inc are the inverse operations here

def deal_into_new(xmul, off):
    return (-xmul % decksize, (-off - 1) % decksize)

def cut(n, xmul, off):
    return (xmul, (off + n) % decksize)

def find_inverse(smaller, larger):
    # assumes 0 < smaller < larger
    # return a pair (h, k) s.t. h*smaller == k*larger + gcd(smaller, larger)
    # also, 0 <= h < larger and 0 <= k < smaller
    (quot, rem) = divmod(larger, smaller)
    if rem == 0:
        return (1, 0)
    (subans, k) = find_inverse(rem, smaller)
    # So now, some algebra:
    # subans*rem = k*smaller + gcd
    # subans*(larger - quot*smaller) = k*smaller + gcd
    # -k*smaller - quot*subans*smaller = -subans*larger + gcd
    # larger*smaller -k*smaller - quot*subans*smaller ==
    #        smaller*larger - subans*larger + gcd
    # (larger - k - quot*subans)*smaller == (smaller - subans)*larger + gcd
    #
    # Note that subans < smaller and k < rem
    # therefore, guaranteed that k + quot*subans < larger
    #
    return (larger - k - quot*subans, smaller - subans)

def deal_inc(n, xmul, off):
    ninv = find_inverse(n, decksize)[0]
    return (xmul * ninv % decksize, off * ninv % decksize)


decksize = 119315717514047
xmul_, off_ = (1, 0)
for line in reversed(data):
    m = re.match('deal into new stack', line)
    if m:
        xmul_, off_ = deal_into_new(xmul_, off_)
        continue
    m = re.match(r'cut (-?\d+)', line)
    if m:
        xmul_, off_ = cut(int(m.group(1)), xmul_, off_)
        continue
    m = re.match(r'deal with increment (-?\d+)', line)
    if m:
        xmul_, off_ = deal_inc(int(m.group(1)), xmul_, off_)
        continue
    raise Exception("Bad input %r" % (line,))

print((xmul_, off_))

def combine_two_actions(xmul1, off1, xmul2, off2):
    # Doing (xm1 off1)       (xm2 off2)
    #       ( 0    1 )   .   ( 0    1 )
    return ((xmul1 * xmul2) % decksize,
            (xmul1 * off2 + off1) % decksize)

def action_to_power(power, xmul, off):
    if power == 0:
        return (1, 0)
    if power % 2 == 1:
        (a, b) = action_to_power(power - 1, xmul, off)
        return combine_two_actions(a, b, xmul, off)
    (a, b) = action_to_power(power // 2, xmul, off)
    return combine_two_actions(a, b, a, b)

xmulp, offp = action_to_power(101741582076661, xmul_, off_)

print((xmulp, offp))

print((2020*xmulp + offp) % decksize)
