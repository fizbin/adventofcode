import re


def process(state, line):
    m = re.match('hlf (.)', line)
    if m:
        if m.group(1) == 'a':
            return [state[0] // 2, state[1], state[2]+1]
        return [state[0], state[1] // 2, state[2]+1]
    m = re.match('tpl (.)', line)
    if m:
        if m.group(1) == 'a':
            return [state[0] * 3, state[1], state[2]+1]
        return [state[0], state[1] * 3, state[2]+1]
    m = re.match('inc (.)', line)
    if m:
        if m.group(1) == 'a':
            return [state[0] + 1, state[1], state[2]+1]
        return [state[0], state[1] + 1, state[2]+1]
    m = re.match('jmp (\S+)', line)
    if m:
        return [state[0], state[1], state[2] + int(m.group(1))]
    m = re.match('jie ([ab]), *(\S+)', line)
    if m:
        val = state['ab'.index(m.group(1))]
        if (val % 2) == 0:
            return [state[0], state[1], state[2] + int(m.group(2))]
        return [state[0], state[1], state[2] + 1]
    m = re.match('jio ([ab]), *(\S+)', line)
    if m:
        val = state['ab'.index(m.group(1))]
        if val == 1:
            return [state[0], state[1], state[2] + int(m.group(2))]
        return [state[0], state[1], state[2] + 1]

state = [1, 0, 0]  # a, b, ip
data = list(open('aoc23.in.txt'))
while state[2] < len(data):
    state = process(state, data[state[2]])
print(state)
