import sys
import re
import time

def doit():
    with open('aoc25.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = [x.strip() for x in f]

    pubkey1 = int(data[0])
    pubkey2 = int(data[1])
    exp1 = 0
    pow1 = 1
    while (pow1 != pubkey1):
        pow1 *= 7
        pow1 %= 20201227
        exp1 += 1
    print(exp1)
    print(pow(pubkey2, exp1, 20201227))

if __name__ == '__main__':
    doit()
