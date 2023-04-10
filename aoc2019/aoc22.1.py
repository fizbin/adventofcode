import sys
import re


with open('aoc22.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

def deal_into_new(cards):
    return list(reversed(cards))

def cut(n, cards):
    while n < 0:
        n += len(cards)
    cards = cards[n:] + cards[:n]
    return cards

def deal_inc(n, cards):
    newcards = [0 for _ in cards]
    for (idx, val) in enumerate(cards):
        newcards[(idx * n) % len(cards)] = val
    return newcards


cards = list(range(10007))
for line in data:
    m = re.match('deal into new stack', line)
    if m:
        cards = deal_into_new(cards)
        continue
    m = re.match(r'cut (-?\d+)', line)
    if m:
        cards = cut(int(m.group(1)), cards)
        continue
    m = re.match(r'deal with increment (-?\d+)', line)
    if m:
        cards = deal_inc(int(m.group(1)), cards)
        continue
    raise Exception("Bad input %r" % (line,))

print([x for x in range(10007) if cards[x] == 2019])
