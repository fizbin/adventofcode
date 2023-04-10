from __future__ import print_function

import sys
import re

with open('aoc21.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

def addr(a, b, c):
    return f"reg{c} = reg{a} + reg{b}"
def addi(a, b, c):
    return f"reg{c} = reg{a} + {b}"
def mulr(a, b, c):
    return f"reg{c} = reg{a}*reg{b}"
def muli(a, b, c):
    return f"reg{c} = reg{a}*{b}"
def banr(a, b, c):
    return f"reg{c} = reg{a} & reg{b}"
def bani(a, b, c):
    return f"reg{c} = reg{a} & {b}"
def borr(a, b, c):
    return f"reg{c} = reg{a} | reg{b}"
def bori(a, b, c):
    return f"reg{c} = reg{a} | {b}"
def setr(a, b, c):
    return f"reg{c} = reg{a}"
def seti(a, b, c):
    return f"reg{c} = {a}"
def gtir(a, b, c):
    return f"reg{c} = 1 if {a} > reg{b} else 0"
def gtri(a, b, c):
    return f"reg{c} = 1 if reg{a} > {b} else 0"
def gtrr(a, b, c):
    return f"reg{c} = 1 if reg{a} > reg{b} else 0"
def eqir(a, b, c):
    return f"reg{c} = 1 if {a} == reg{b} else 0"
def eqri(a, b, c):
    return f"reg{c} = 1 if reg{a} == {b} else 0"
def eqrr(a, b, c):
    return f"reg{c} = 1 if reg{a} == reg{b} else 0"


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


iploc = None
state = [0] * 6

m = re.match('#ip (\d)', data.pop(0))
iploc = int(m.group(1))

linefuncs = []
modcode = f'''
def runit(state):
    execed = 0
    (reg0, reg1, reg2, reg3, reg4, reg5) = state
    while reg{iploc} >= 0 and reg{iploc} < {len(data)}:
'''

for (n, ln) in enumerate(data):
    m = re.match(r'([a-z]+) (\d+) (\d+) (\d+)', ln)
    code = f"""
        if reg{iploc} == {n}:
            {all_ops[m.group(1)](m.group(2), m.group(3), m.group(4))}
"""
    if n == 28:
        code += f"""
            print("ip=%d {ln.strip()} %s (%d)" % (
                reg{iploc},
                str((reg0, reg1, reg2, reg3, reg4, reg5)),
                execed + 1))
"""
    code += f"            reg{iploc} += 1\n"
    code += f"            execed += 1\n"

    modcode += code

modcode += '    return (reg0, reg1, reg2, reg3, reg4, reg5)\n'
gl = {}
exec(modcode, gl)
if len(sys.argv) > 2:
    state_stuff = map(int, sys.argv[2:])
    for (n, s) in enumerate(state_stuff):
        state[n] = s

r = gl['runit'](state)
print("")
print(r)
