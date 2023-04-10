import sys
import re

# each cups points to "nextval"
def do_one_turn(cups, curcup):
    ncups = len(cups)
    a1 = cups[curcup]
    a2 = cups[a1]
    a3 = cups[a2]
    nxtcur = cups[a3]
    tcup = curcup - 1
    while tcup in (0, a1, a2, a3):
        tcup -= 1
        if tcup <= 0:
            tcup = ncups - 1
    cups[a3] = cups[tcup]
    cups[tcup] = a1
    cups[curcup] = nxtcur
    return (cups, nxtcur)

def printit(cups, curcup):
    pcup = 1
    print(1, end=' ')
    while cups[pcup] != 1:
        pcup = cups[pcup]
        print(pcup, end=' ')
    print(curcup)


def doit(isPart2):
    data = '496138527'
    if len(sys.argv) >= 2:
        data = sys.argv[1]
    cups = [int(x) for x in data]
    if isPart2:
        nxtcup = max(cups) + 1
        while len(cups) < 1000000:
            cups.append(nxtcup)
            nxtcup += 1
    cup_data = cups
    cups = [0] * (1 + max(cup_data))
    for idx in range(-1, len(cup_data) - 1):
        cups[cup_data[idx]] = cup_data[idx+1]
    curcup = cup_data[0]
    limit = 100
    if isPart2:
        limit = 10000000
    for turn in range(limit):
        # printit(cups, curcup)
        (cups, curcup) = do_one_turn(cups, curcup)

    if isPart2:
        cupA = cups[1]
        cupB = cups[cupA]
        print(cupA*cupB)
    else:
        pcup = 1
        while cups[pcup] != 1:
            pcup = cups[pcup]
            print(pcup, end='')
        print()

if __name__ == '__main__':
    doit(False)
    doit(True)
