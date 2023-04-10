from aoc_util import numbers, get_data_lines
import re

def part1(data):
    mvals = {}
    for line in data:
        name = line.split(':')[0]
        n = numbers(line)
        if n:
            mvals[name] = n[0]
    while 'root' not in mvals:
        for line in data:
            m = re.match(r'(\S+): (\S+) ([-+/*]) (\S+)', line)
            if m:
                if m.group(2) in mvals and m.group(4) in mvals:
                    ans = eval(f"int({mvals[m.group(2)]} {m.group(3)} {mvals[m.group(4)]})")
                    mvals[m.group(1)] = ans
    print(mvals['root'])

def part2(data):
    mvals = {}
    for (idx, line) in enumerate(data):
        if line.startswith('root:'):
            data[idx] = re.sub(r'[-+/*]', '==', line)
    for line in data:
        name = line.split(':')[0]
        n = numbers(line)
        if n:
            mvals[name] = n[0]
    mvals['humn'] = 'humn'
    while 'root' not in mvals:
        for line in data:
            m = re.match(r'(\S+): (\S+) ([-+/*]|==) (\S+)', line)
            if m:
                if m.group(2) in mvals and m.group(4) in mvals:
                    fun = (f"   ({mvals[m.group(2)]} {m.group(3)} {mvals[m.group(4)]})")
                    if isinstance(mvals[m.group(2)], (int,float)) and isinstance(mvals[m.group(4)], (int,float)):
                        ans = eval(fun)
                    else:
                        ans = fun
                    mvals[m.group(1)] = ans
    # print(mvals['root'])
    m = re.match(r'^(?:int|   )\((.*) == *([0-9.]*)', mvals['root'])
    fn = eval("lambda humn: " + m.group(1))
    target = float(m.group(2))
    a = 0
    b = int(10*target)
    # print("a", fn(a))
    # print("b", fn(b))
    while abs(b - a) > 1:
        c = (a+b)//2
        if fn(c) < target:
            b = c
        else:
            a = c
        # print("~", c, fn(c))

    print('target', target)
    for x in range(a-2, b+3):
        print(x, fn(x))


def main():
    data = get_data_lines(21)

    part1(data)
    part2(data)

if __name__ == '__main__':
    main()
