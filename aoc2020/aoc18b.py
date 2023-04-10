import sys
import re

def evalstr(s):
    m = re.match(r'(.*)\(([^()]*)\)(.*)', s)
    while m:
        s = m.group(1) + str(evalstr(m.group(2))) + m.group(3)
        m = re.match(r'(.*)\(([^()]*)\)(.*)', s)
    m = re.match(r'(.*?)(\d+) *\+ *(\d+)(.*)', s)
    while m:
        s = m.group(1) + str(int(m.group(2)) + int(m.group(3))) + m.group(4)
        m = re.match(r'(.*?)(\d+) *\+ *(\d+)(.*)', s)
    if '+' in s:
        raise Exception(s)
    while not re.match(r' *\d+ *$', s):
        print('at', s)
        m = re.match(r' *(\d+ *[+*] *\d+)(.*)', s)
        s = str(eval(m.group(1))) + m.group(2)
    return int(s.strip())

def doit():
    with open('aoc18.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)
    print(sum(evalstr(ln) for ln in data))
    

if __name__ == '__main__':
    doit()
