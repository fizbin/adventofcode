import numpy as np
import sys

goal = int(sys.argv[1]) if len(sys.argv) > 1 else 29000000
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31]

boxnum = 1
score = 1
contribution = dict((x, 1) for x in primes)

while score*10 < goal:
    ratios = {}
    for p in primes:
        u = contribution[p]
        ratios[p] = (p*u + 1)/(p*u)
    maxp = max(primes, key=ratios.get)
    score //= contribution[maxp]
    contribution[maxp] *= maxp
    contribution[maxp] += 1
    score *= contribution[maxp]
    boxnum *= maxp

print(boxnum)
print(contribution)

bigarray = np.zeros(shape=(boxnum,))

for j in range(1, boxnum):
    bigarray[np.arange(0, boxnum, j)] += j

bigarray[0] = 0
print(np.flatnonzero(bigarray*10 >= goal).min())
