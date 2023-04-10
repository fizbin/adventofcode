#!/usr/bin/python
import sys
import collections
import queue
import random


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
    with open('aoc23.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(f)

    for line in data:
        prog = [int(x) for x in line.split(',')]

    inqueues = [queue.Queue() for _ in range(50)]
    outq = queue.Queue()
    nat_buffer = [None, None]

    class NIC:
        def __init__(self, n):
            self.n = n
            self.comp = IntCode(prog)
            self.inqueue = inqueues[n]
            # A NIC gets marked idle when it gets a -1 back from input, and
            # is marked as non-idle when it sends out something, or gets
            # real input
            self.idle = False
            self.outbuf = []

        def is_idle(self):
            return (self.inqueue.empty() and self.idle)

        def do_output(self, val):
            self.outbuf.append(val)
            while len(self.outbuf) >= 3:
                (dest, xcoord, ycoord) = self.outbuf[0:3]
                del self.outbuf[0:3]
                print("Emit (%d, %d) to %d from %d" %
                      (xcoord, ycoord, dest, self.n))
                if dest == 255:
                    nat_buffer[:] = [xcoord, ycoord]
                else:
                    inqueues[dest].put(xcoord)
                    inqueues[dest].put(ycoord)

        def run(self):
            return self.comp.evalprog(lambda: self.inqueue.get(False),
                                      self.do_output)

    nics = []
    for thread_num in range(50):
        nics.append(NIC(thread_num))
        nics[-1].inqueue.put(thread_num)

    # random.shuffle(nics)

    prevy = 0

    while True:
        while not all(myqueue.empty() for myqueue in inqueues):
            for nic in nics:
                if not nic.inqueue.empty():
                    try:
                        nic.run()
                        print("NIC %d quit" % (nic.n,))
                        sys.exit(2)
                    except queue.Empty:
                        nic.inqueue.put(-1)
                        try:
                            nic.run()
                            print("NIC %d quit" % (nic.n,))
                            sys.exit(2)
                        except queue.Empty:
                            # expected this. Move along...
                            pass
        if prevy == nat_buffer[1]:
            print("done", prevy)
            break
        prevy = nat_buffer[1]
        print("NAT forwarding", nat_buffer)
        inqueues[0].put(nat_buffer[0])
        inqueues[0].put(nat_buffer[1])
