import sys
import re

def evalstr(s):
    while '(' in s:
        m = re.match(r'(.*\() *(\d+ *[+*] *\d+)(.*)', s)
        if m:
            s = m.group(1) + str(eval(m.group(2))) + m.group(3)
        s = re.sub(r'(.*) *\( *(\d+) *\) *(.*)', r'\1 \2 \3', s)
    while not re.match(r' *\d+ *$', s):
        m = re.match(r' *(\d+ *[+*] *\d+)(.*)', s)
        s = str(eval(m.group(1))) + m.group(2)
    return int(s.strip())

def doit():
    with open('aoc18.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)
    print(sum(evalstr(ln) for ln in data))
    

if __name__ == '__main__':
    doit()
