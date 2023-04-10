#!/usr/bin/python
import sys
import collections
import re
import queue


class ZeroList(collections.UserList):
    def __getitem__(self, key):
        if key >= len(self.data):
            return 0
        return super().__getitem__(key)

    def __setitem__(self, key, value):
        if key >= len(self.data):
            if len(self.data) < 100:
                self.data.extend([0] * 100)
            while len(self.data) <= key:
                self.data.extend([0] * len(self.data))
        super().__setitem__(key, value)


class IntCode():
    def __init__(self, iprog):
        self.memory = ZeroList(iprog)
        self.ip = 0
        self.relbase = 0

    def get_param(self, param_num):
        imode = self.memory[self.ip] // (10 * 10 ** param_num)
        val = self.memory[self.ip + param_num]
        if imode % 10 == 0:
            return self.memory[val]
        if imode % 10 == 1:
            return val
        if imode % 10 == 2:
            return self.memory[val + self.relbase]
        print("BAD get imode %d at idx %d" % (imode, self.ip))
        sys.exit(2)

    def set_param(self, param_num, set_to):
        imode = self.memory[self.ip] // (10 * 10 ** param_num)
        val = self.memory[self.ip + param_num]
        if imode % 10 == 0:
            self.memory[val] = set_to
            return
        if imode % 10 == 2:
            self.memory[val + self.relbase] = set_to
            return
        print("BAD set imode %d at idx %d" % (imode, self.ip))
        sys.exit(2)

    def evalprog(self, inputf, outputf):
        for out_thing in self.yieldprog(inputf):
            outputf(out_thing)

    def yieldprog(self, inputf):
        while True:
            if self.memory[self.ip] % 100 == 1:
                self.set_param(3, self.get_param(1) + self.get_param(2))
                self.ip += 4
            elif self.memory[self.ip] % 100 == 2:
                self.set_param(3, self.get_param(1) * self.get_param(2))
                self.ip += 4
            elif self.memory[self.ip] % 100 == 3:
                x = inputf()
                self.set_param(1, x)
                self.ip += 2
            elif self.memory[self.ip] % 100 == 4:
                yield self.get_param(1)
                self.ip += 2
            elif self.memory[self.ip] % 100 == 5:
                p1 = self.get_param(1)
                if p1:
                    self.ip = self.get_param(2)
                else:
                    self.ip += 3
            elif self.memory[self.ip] % 100 == 6:
                p1 = self.get_param(1)
                if not p1:
                    self.ip = self.get_param(2)
                else:
                    self.ip += 3
            elif self.memory[self.ip] % 100 == 7:
                p1 = self.get_param(1)
                p2 = self.get_param(2)
                self.set_param(3, 1 if p1 < p2 else 0)
                self.ip += 4
            elif self.memory[self.ip] % 100 == 8:
                p1 = self.get_param(1)
                p2 = self.get_param(2)
                self.set_param(3, 1 if p1 == p2 else 0)
                self.ip += 4
            elif self.memory[self.ip] % 100 == 9:
                p1 = self.get_param(1)
                self.relbase += p1
                self.ip += 2
            elif self.memory[self.ip] % 100 == 99:
                self.ip += 1
                break
            else:
                print("BAD CODE %d AT %d" % (self.memory[self.ip], self.ip))
                sys.exit(2)
        return


if __name__ == '__main__':
    with open('aoc17.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(f)

    for line in data:
        prog = [int(x) for x in line.split(',')]

    def do_no_input():
        raise Exception('asked for input')

    outs = [[]]
    for output in IntCode(prog).yieldprog(do_no_input):
        if output == 10:
            outs.append([])
        else:
            outs[-1].append(chr(output))

    scaffolds = set()
    robostart = None
    robodir = None
    for (x, out) in enumerate(outs):
        for (y, ch) in enumerate(out):
            if ch == '#':
                scaffolds.add(x + y*(0+1j))
            if ch in '<>^v':
                robostart = x + y*(0+1j)
                robodir = {'>': 0+1j, '<': 0-1j, '^': -1+0j, 'v': 1+0j}[ch]
    for out in outs:
        print(''.join(out))

    def find_scaffold_dirs(spot):
        retval = []
        for reach in [0+1j, 0-1j, 1+0j, -1+0j]:
            if (reach + spot) in scaffolds:
                retval.append(reach)
        return tuple(retval)

    fullstr = ''
    roboz = robostart
    while True:
        for target_dir in find_scaffold_dirs(roboz):
            if target_dir == -robodir:
                continue
            fullstr += {0+1j: 'L', 0-1j: 'R'}[target_dir / robodir]
            robodir = target_dir
            break
        else:
            break
        while (roboz + robodir) in scaffolds:
            fullstr += '1'
            roboz += robodir

    fullstr = re.sub(r'(1+)', lambda m: ',' + str(len(m.group(1))) + ',',
                     fullstr)
    fullstr = re.sub('^,', '', fullstr)
    print(fullstr)
    # try naive breaking it up...
    workstr = fullstr
    stopAidx = 0
    while fullstr[0:stopAidx+1] in workstr[stopAidx+1:]:
        stopAidx += 1
    stopAidx = 18
    grpA = fullstr[0:stopAidx]
    while grpA in workstr:
        loc = workstr.find(grpA)
        workstr = workstr[:loc] + 'A' * len(grpA) + workstr[loc+len(grpA):]
    startBidx = 0
    while workstr[startBidx] in 'ABC':
        startBidx += 1
    stopBidx = startBidx
    while workstr[startBidx:stopBidx+1] in workstr[stopBidx+1:]:
        stopBidx += 1
    grpB = fullstr[startBidx:stopBidx]
    while grpB in workstr:
        loc = workstr.find(grpB)
        workstr = workstr[:loc] + 'B' * len(grpB) + workstr[loc+len(grpB):]
    startCidx = 0
    while workstr[startCidx] in 'ABC':
        startCidx += 1
    stopCidx = startCidx
    while workstr[startCidx:stopCidx+1] in workstr[stopCidx+1:]:
        stopCidx += 1
    grpC = fullstr[startCidx:stopCidx]
    while grpC in workstr:
        loc = workstr.find(grpC)
        workstr = workstr[:loc] + 'C' * len(grpC) + workstr[loc+len(grpC):]

    print("workstr", workstr)
    print("grpA", grpA)
    print("grpB", grpB)
    print("grpC", grpC)

    masterprog = ''
    while workstr:
        if workstr[0] == 'A':
            masterprog += 'A,'
            workstr = workstr[len(grpA):]
        elif workstr[0] == 'B':
            masterprog += 'B,'
            workstr = workstr[len(grpB):]
        elif workstr[0] == 'C':
            masterprog += 'C,'
            workstr = workstr[len(grpC):]
    print('masterprog', masterprog)

    prog[0] = 2
    inqueue = queue.Queue()
    instring = (re.sub(',$', '\n', masterprog) +
                re.sub(',$', '\n', grpA) +
                re.sub(',$', '\n', grpB) +
                re.sub(',$', '\n', grpC) + 'n\n')
    for ch in instring:
        inqueue.put(ord(ch))

    poutput = None
    for output in IntCode(prog).yieldprog(lambda: inqueue.get(False)):
        if poutput is not None:
            print(chr(poutput), end='')
        poutput = output
    print(poutput)
