#!/usr/bin/env python

from aoc_util import get_data_lines, numbers
from collections import Counter
import re

CARD_ORDER_1 = "23456789TJQKA"
CARD_ORDER_2 = "J23456789TQKA"


def hand_kind(hand):
    chset = set(hand)
    chcount = Counter(hand)
    if len(chset) == 1:
        return 7
    if len(chset) == 2:
        if chcount.most_common(1)[0][1] == 4:
            return 6
        return 5
    if len(chset) == 3:
        if chcount.most_common(1)[0][1] == 3:
            return 4
        return 3
    if len(chset) == 4:
        return 2
    return 1


def hand_key(hand):
    tot = hand_kind(hand)
    for ch in hand:
        tot *= 100
        tot += 1 + CARD_ORDER_1.index(ch)
    return tot


def hand_kind2(hand):
    njokers = len([h for h in hand if h == "J"])
    chcount = Counter(h for h in hand if h != "J")
    chset = set(h for h in hand if h != "J")
    if len(chset) <= 1:
        return 7
    if len(chset) == 2:
        if chcount.most_common(1)[0][1] + njokers == 4:
            return 6
        return 5
    if len(chset) == 3:
        if chcount.most_common(1)[0][1] + njokers == 3:
            return 4
        return 3
    if len(chset) == 4:
        return 2
    return 1


def hand_key2(hand):
    tot = hand_kind2(hand)
    for ch in hand:
        tot *= 100
        tot += 1 + CARD_ORDER_2.index(ch)
    return tot


data = get_data_lines(7)
hands = [ln.split() for ln in data]
hands.sort(key=lambda x: hand_key(x[0]))
val = sum((idx + 1) * int(bid) for (idx, (_, bid)) in enumerate(hands))
print(val)
hands.sort(key=lambda x: hand_key2(x[0]))
val = sum((idx + 1) * int(bid) for (idx, (_, bid)) in enumerate(hands))
print(val)
