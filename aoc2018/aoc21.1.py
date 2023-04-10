from __future__ import print_function

import sys
import re

with open('aoc21.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
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


all_ops = {
    'addr': addr,
    'addi': addi,
    'mulr': mulr,
    'muli': muli,
    'banr': banr,
    'bani': bani,
    'borr': borr,
    'bori': bori,
    'setr': setr,
    'seti': seti,
    'gtir': gtir,
    'gtri': gtri,
    'gtrr': gtrr,
    'eqir': eqir,
    'eqri': eqri,
    'eqrr': eqrr,
    }


IP = 6
iploc = None
state = [0] * 7

m = re.match('#ip (\d)', data.pop(0))
iploc = int(m.group(1))

if len(sys.argv) > 2:
    state_stuff = map(int, sys.argv[2:])
    for (n, s) in enumerate(state_stuff):
        state[n] = s
    state[IP] = state[iploc]

execed = 0
while state[IP] >= 0 and state[IP] < len(data):
    line = data[state[IP]]
    state[iploc] = state[IP]
    m = re.match(r'([a-z]+) (\d+) (\d+) (\d+)', line)
    state1 = list(state)
    all_ops[m.group(1)](state, int(m.group(2)), int(m.group(3)), int(m.group(4)))
    print("ip=%d %s %s %s (%d)" % (state[IP], str(state1[:IP]),
                                   line.strip(), str(state[:IP]),
                                   execed + 1))
    state[IP] = state[iploc]
    state[IP] += 1
    state[iploc] = state[IP]
    execed += 1

print("")
print(state[:IP], execed)
