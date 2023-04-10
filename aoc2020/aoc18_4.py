import sys
import re

def to_rpn(s, isPart2):
    # returns rpn_string, rest_of_string
    deferred = ''
    out = ''
    s1 = s
    m = re.match(r'\s*(?:(\d+)|([()+*]))(.*)', s)
    while m:
        (num, op, s) = m.groups()
        if num:
            out += num + ' '
        elif op == '(':
            (subrpn, rest) = to_rpn(s, isPart2)
            out += subrpn + ' '
            s = rest
        elif op == ')':
            return (out + deferred, s)
        elif op == '+' and isPart2:
            deferred = '+ ' + deferred
        else:
            out += deferred
            deferred = op + ' '
        m = re.match(r'\s*(?:(\d+)|([()+*]))(.*)', s)
    assert False, f"s {s} s1 {s1}"

def eval_rpn(s):
    #print(repr(s))
    stack = []
    for (num, op) in re.findall(r'\s*(\d+)|([()+*])', s):
        if num:
            stack.append(int(num))
        if op == '+':
            stack[-2:] = [stack[-2] + stack[-1]]
        if op == '*':
            stack[-2:] = [stack[-2] * stack[-1]]
    assert len(stack) == 1, s
    return stack[0]

def doit():
    with open('aoc18.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)
    print(sum(eval_rpn(to_rpn(ln + ')', False)[0]) for ln in data))
    print(sum(eval_rpn(to_rpn(ln + ')', True)[0]) for ln in data))

if __name__ == '__main__':
    doit()
