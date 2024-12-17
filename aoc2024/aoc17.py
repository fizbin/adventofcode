from aoc_util import get_data
import aoc_util
import heapq
import re
import copy
import itertools
import functools

data = get_data(17)

output = []

def adv(st, arg):
    st['A'] = st['A'] // (2**arg)
def bst(st, arg):
    st['B'] = arg%8
def bxl(st, arg):
    st['B'] ^= arg
def jnz(st, arg):
    if st['A']:
        st['I'] = arg - 2
def bxc(st, arg):
    st['B'] = st['B']^st['C']
def out(st, arg):
    st['o'].append(arg%8)
def bdv(st, arg):
    st['B'] = st['A'] // (2**arg)
def cdv(st, arg):
    st['C'] = st['A'] // (2**arg)

opcodes = {
    0: (True, adv),
    1: (False, bxl),
2: (True, bst),
3: (False, jnz),
4: (False, bxc),
5: (True, out),
6: (True, bdv),
7: (True, cdv)
}

m = re.match(r"Register A: (\d+)\s*Register B: (\d+)\s*Register C: (\d+)\s*Program: ([,\d]+)", data)
st = {'A': int(m.group(1)), 'B': int(m.group(2)), 'C': int(m.group(3)), 'o': [], 'I': 0}
prog = [int(x) for x in m.group(4).split(',')]

while 0 <= st['I'] < len(prog)-1:
    instr = prog[st['I']]
    argv = prog[st['I']+1]
    if opcodes[instr][0] and argv >= 4:
        if argv == 4:
            argv = st['A']
        elif argv == 5:
            # print("DBG2", st['I'])
            argv = st['B']
        elif argv == 6:
            argv = st['C']
        else:
            assert(False)
    opcodes[instr][1](st, argv)
    st['I'] += 2
    # print("DBG", str(st))

print(",".join(str(y) for y in st['o']))


def runprog(aval):
    st = {'A': aval, 'B': 0, 'C': 0, 'o': [], 'I': 0}
    while 0 <= st['I'] < len(prog)-1:
        instr = prog[st['I']]
        argv = prog[st['I']+1]
        if opcodes[instr][0] and argv >= 4:
            if argv == 4:
                argv = st['A']
            elif argv == 5:
                # print("DBG2", st['I'])
                argv = st['B']
            elif argv == 6:
                argv = st['C']
            else:
                assert(False)
        opcodes[instr][1](st, argv)
        st['I'] += 2
    return st['o']

aexp = 0
progout = []
avals = [0]
correct = 0
while progout != prog:
    icorrect = correct
    navals = []
    for iaval in avals:
        for nbit in range(1024):
            aval = (iaval & ~(1023 * 8**aexp)) ^ nbit * (8**aexp)
            progout = runprog(aval)
            if progout == prog:
                print(aval)
                exit(0)
            if progout[:correct+1] == prog[:correct+1]:
                bst = progout
                # print("DBG", correct, aval, nbit, progout)
                navals.append(aval)
    if not navals:
        break
    avals = navals
    aexp += 1
    avals = sorted(set(x % (8**(aexp)) for x in avals))
    correct = max(0, aexp - 1)
    # print("DBG: ", bst[:correct+1], correct, len(avals), (min(avals), max(avals)), 8**aexp)
