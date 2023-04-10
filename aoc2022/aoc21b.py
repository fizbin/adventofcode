from aoc_util import get_data_lines
import re

def part1(data):
    mvals = dict(line.split(':') for line in data)
    def getm(m):
        s = mvals[m]
        m = re.match(r' *(\S+) ([-+/*]) (\S+)', s)
        if m:
            s = f"({getm(m.group(1))}) {m.group(2)} ({getm(m.group(3))})"
        return s
    print(int(eval(getm('root'))))

def part2(data):
    mvals = dict(line.split(': ') for line in data)
    def getm(monk):
        # Like before, but "humn" returns a var, and we change the operation
        # on "root" to "-" and then solve for 0
        if monk == 'humn':
            return 'x'
        s = mvals[monk]
        m = re.match(r'(\S+) ([-+/*]) (\S+)', s)
        if m:
            op = m.group(2)
            a = getm(m.group(1))
            b = getm(m.group(3))
            if monk == 'root':
                op = '-'
            s = f"({a} {op} {b})"
        return s
    fn = eval("lambda x: (" + getm('root') + ")")
    a = 0
    b = 10000000000000
    while abs(b - a) > 0.3:
        c = (a+b)/2
        if fn(c) < 0:
            b = c
        else:
            a = c
    for x in range(int(a-10), int(b+10)):
        if abs(fn(x)) < 0.00001:
            print(x)
            break
    else:
        print("ERR: no zero found")


def main():
    data = get_data_lines(21)

    part1(data)
    part2(data)

if __name__ == '__main__':
    main()
