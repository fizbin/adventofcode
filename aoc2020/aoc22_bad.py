import sys
import re
import functools

def play_game(recurse, deck1a, deck2a):
    # print(f"{lvl} {len(deck1a)} {len(deck2a)}")
    deck1 = list(deck1a)
    deck2 = list(deck2a)
    visions = set([])
    roundn = 0
    while deck1 and deck2:
        roundn += 1
        if (roundn > 100000 == 0):
            roundn = 0
            print(f"alert {deck1} {deck2} @ {lvl} {deck1a} {deck2a}")
        vision = tuple(deck1 + [0] + deck2)
        if vision in visions:
            deck2 = []
            print("broke out")
            break
        visions.add(vision)
        top1 = deck1.pop(0)
        top2 = deck2.pop(0)
        if recurse and top1 >= len(deck1) and top2 >= len(deck2):
            (p1wins, _) = play_game(recurse,
                                    tuple(deck1[0:top1]),
                                    tuple(deck2[0:top2]))
        else:
            p1wins = top1 > top2
        if p1wins:
            deck1.append(top1)
            deck1.append(top2)
        else:
            deck2.append(top2)
            deck2.append(top1)
    print(f"game over [{deck1}] [{deck2}]")
    return (deck1, deck2)

def doit(recurse):
    with open('aoc22.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = f.read().split('\n\n')
    data[0] = re.sub(r'[Pp]layer \d:', '', data[0])
    data[1] = re.sub(r'[Pp]layer \d:', '', data[1])
    deck1 = [int(x) for x in re.findall(r'\d+', data[0])]
    deck2 = [int(x) for x in re.findall(r'\d+', data[1])]

    (deck1, deck2) = play_game(recurse, tuple(deck1), tuple(deck2))
    winning_deck = deck1 or deck2
    print(sum(mult * card for (mult, card) in zip(range(1, 99), reversed(winning_deck))))

if __name__ == '__main__':
    #doit(False)
    doit(True)
