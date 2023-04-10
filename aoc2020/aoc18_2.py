import sys
import re

class P:
    def __init__(self, val):
        self._val = val
    def __truediv__(self, other):
        return P(self._val + other._val)
    def __sub__(self, other):
        return P(self._val * other._val)
    def __mul__(self, other):
        return P(self._val * other._val)

def evalstr(s, isPart2):
    s = re.sub(r'(\d+)', r'P(\1)', s)
    s = re.sub(r'[+]', r'/', s)
    if isPart2:
        s = re.sub(r'[*]', r'-', s)
    return eval(s)._val

def doit():
    with open('aoc18.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)
    print(sum(evalstr(ln, False) for ln in data))
    print(sum(evalstr(ln, True) for ln in data))
    

if __name__ == '__main__':
    doit()
