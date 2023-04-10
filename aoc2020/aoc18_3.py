import sys
import re

def evalstr(s, isPart2):
    opstack = []
    outstack = []
    def apply_top_op():
        if opstack[-1] == '+':
            outstack[-2:] = [outstack[-2] + outstack[-1]]
        elif opstack[-1] == '*':
            outstack[-2:] = [outstack[-2] * outstack[-1]]
        else:
            raise Exception(f"Bad state; opstack {opstack} outstack {outstack}")
        opstack.pop()

    for m in re.finditer(r'([()])|(\d+)|([+*])', s):
        if m.group(1) == '(':
            opstack.append('(')
        elif m.group(1) == ')':
            while opstack[-1] != '(':
                apply_top_op()
            opstack.pop()
        elif m.group(2):
            outstack.append(int(m.group(2)))
        elif isPart2:
            while opstack and m.group(3) == '*' and opstack[-1] == '+':
                apply_top_op()
            opstack.append(m.group(3))
        else:
            while opstack and opstack[-1] != '(':
                apply_top_op()
            opstack.append(m.group(3))
    while opstack:
        apply_top_op()
    assert len(outstack) == 1, f"opstack {opstack} outstack {outstack}"
    return outstack[0]

def doit():
    with open('aoc18.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)
    print(sum(evalstr(ln, False) for ln in data))
    print(sum(evalstr(ln, True) for ln in data))

if __name__ == '__main__':
    doit()
