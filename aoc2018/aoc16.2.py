import re
import copy

with open('aoc16.in.txt') as f:
    data = list(f)

def addr(s, a, b, c):
    s[c] = s[a] + s[b]
def addi(s, a, b, c):
    s[c] = s[a] + b
def mulr(s, a, b, c):
    s[c] = s[a]*s[b]
def muli(s, a, b, c):
    s[c] = s[a]*b
def banr(s, a, b, c):
    s[c] = s[a] & s[b]
def bani(s, a, b, c):
    s[c] = s[a] & b
def borr(s, a, b, c):
    s[c] = s[a] | s[b]
def bori(s, a, b, c):
    s[c] = s[a] | b
def setr(s, a, b, c):
    s[c] = s[a]
def seti(s, a, b, c):
    s[c] = a
def gtir(s, a, b, c):
    s[c] = 1 if a > s[b] else 0
def gtri(s, a, b, c):
    s[c] = 1 if s[a] > b else 0
def gtrr(s, a, b, c):
    s[c] = 1 if s[a] > s[b] else 0
def eqir(s, a, b, c):
    s[c] = 1 if a == s[b] else 0
def eqri(s, a, b, c):
    s[c] = 1 if s[a] == b else 0
def eqrr(s, a, b, c):
    s[c] = 1 if s[a] == s[b] else 0


all_ops = [addr, addi, mulr, muli, banr, bani, borr, bori,
           setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]


def check_op(op, a, b, c, before, after):
    s = list(before)
    try:
        op(s, a, b, c)
        return s == list(after)
    except IndexError:
        return False


before = (0, 0, 0, 0)
argline = (0, 0, 0, 0)
count = 0
possibilities = {}
prog = []
for line in data:
    if line.startswith('Before:'):
        m = re.match(r'.*\[(\d+), *(\d+), *(\d+), *(\d+) *\]', line)
        before = tuple(int(x) for x in m.group(1, 2, 3, 4))
    elif line.startswith('After:'):
        m = re.match(r'.*\[(\d+), *(\d+), *(\d+), *(\d+) *\]', line)
        after = tuple(int(x) for x in m.group(1, 2, 3, 4))
        found = set(n for (n, op) in enumerate(all_ops)
                    if check_op(op, argline[1], argline[2], argline[3],
                                before, after))
        if argline[0] in possibilities:
            possibilities[argline[0]] &= found
        else:
            possibilities[argline[0]] = found
        before = None
    elif re.match(r'(\d+) *(\d+) *(\d+) *(\d+) *$', line) and before:
        m = re.match(r'(\d+) *(\d+) *(\d+) *(\d+) *$', line)
        argline = tuple(int(x) for x in m.group(1, 2, 3, 4))
    elif re.match(r'(\d+) *(\d+) *(\d+) *(\d+) *$', line):
        m = re.match(r'(\d+) *(\d+) *(\d+) *(\d+) *$', line)
        prog.append(tuple(int(x) for x in m.group(1, 2, 3, 4)))

madechange = True
while madechange:
    madechange = False
    for opcode in possibilities:
        if len(possibilities[opcode]) == 1:
            opnum = min(possibilities[opcode])
            for opcode2 in possibilities:
                if opcode != opcode2 and opnum in possibilities[opcode2]:
                    madechange = True
                    possibilities[opcode2] -= possibilities[opcode]

print(repr(possibilities))
ops = {}
for opcode in possibilities:
    ops[opcode] = all_ops[min(possibilities[opcode])]

s = [0, 0, 0, 0]
for (op, a, b, c) in prog:
    ops[op](s, a, b, c)

print(s)

