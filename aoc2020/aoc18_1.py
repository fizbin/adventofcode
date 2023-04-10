import sys
import re

def evalstr(s, isPart2):
    while '(' in s:
        s = re.sub(r'\(([^()]*)\)',
                   lambda m: str(evalstr(m.group(1), isPart2)),
                   s)
    if isPart2:
        while '+' in s:
            s = re.sub(r'(\d+) *\+ *(\d+)',
                       lambda m: str(int(m.group(1)) + int(m.group(2))),
                       s)
    while '+' in s or '*' in s:
        # do these substitutions 1 at a time to make sure we go
        # strictly left-to-right
        s = re.sub(r'(\d+ *[+*] *\d+)',
                   lambda m: str(eval(m.group(1))),
                   s, 1)
    return int(s.strip())

def doit():
    with open('aoc18.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)
    print(sum(evalstr(ln, False) for ln in data))
    print(sum(evalstr(ln, True) for ln in data))
    

if __name__ == '__main__':
    doit()
