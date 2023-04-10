#!/usr/bin/env python3
import sys

def doit():
    data = '20,9,11,0,1,2'
    if len(sys.argv) >= 2:
        data = sys.argv[1]
    nums = [int(x) for x in data.split(',')
            ]
    numturns = {}
    turn = 0
    lastnum = -1
    for i, n in enumerate(nums):
        numturns[lastnum] = i
        turn = i+1
        lastnum = n
    while turn < 30000000:
        if lastnum in numturns:
            nlastnum = turn - numturns[lastnum]
        else:
            nlastnum = 0
        numturns[lastnum] = turn
        turn += 1
        lastnum = nlastnum
        print(turn, lastnum)
    print(lastnum)

if __name__ == '__main__':
    doit()
