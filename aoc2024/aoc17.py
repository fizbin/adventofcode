from aoc_util import get_data
import re

data = get_data(17)

output = []


def adv(st, arg):
    st["A"] = st["A"] // (2**arg)


def bst(st, arg):
    st["B"] = arg % 8


def bxl(st, arg):
    st["B"] ^= arg


def jnz(st, arg):
    if st["A"]:
        st["I"] = arg - 2


def bxc(st, arg):
    st["B"] = st["B"] ^ st["C"]


def out(st, arg):
    st["o"].append(arg % 8)


def bdv(st, arg):
    st["B"] = st["A"] // (2**arg)


def cdv(st, arg):
    st["C"] = st["A"] // (2**arg)


opcodes = {
    0: (True, adv),
    1: (False, bxl),
    2: (True, bst),
    3: (False, jnz),
    4: (False, bxc),
    5: (True, out),
    6: (True, bdv),
    7: (True, cdv),
}

m = re.match(r"Register A: (\d+)\s*Register B: (\d+)\s*Register C: (\d+)\s*Program: ([,\d]+)", data)
st = {"A": int(m.group(1)), "B": int(m.group(2)), "C": int(m.group(3)), "o": [], "I": 0}
prog = [int(x) for x in m.group(4).split(",")]

while 0 <= st["I"] < len(prog) - 1:
    instr = prog[st["I"]]
    argv = prog[st["I"] + 1]
    if opcodes[instr][0] and argv >= 4:
        if argv == 4:
            argv = st["A"]
        elif argv == 5:
            # print("DBG2", st['I'])
            argv = st["B"]
        elif argv == 6:
            argv = st["C"]
        else:
            assert False
    opcodes[instr][1](st, argv)
    st["I"] += 2
    # print("DBG", str(st))

print("Part 1:", ",".join(str(y) for y in st["o"]))


def runprog(aval):
    st = {"A": aval, "B": 0, "C": 0, "o": [], "I": 0}
    while 0 <= st["I"] < len(prog) - 1:
        instr = prog[st["I"]]
        argv = prog[st["I"] + 1]
        if opcodes[instr][0] and argv >= 4:
            if argv == 4:
                argv = st["A"]
            elif argv == 5:
                argv = st["B"]
            elif argv == 6:
                argv = st["C"]
            else:
                assert False
        opcodes[instr][1](st, argv)
        st["I"] += 2
    return st["o"]


def part2():
    # By inspection of the program, the Nth number output
    # can only depend on the bottom 7+3*n bits of A, and does
    # NOT depend on the bottom 3*(n-1) bits
    # (that is, first number output depends on bottom 10 bits,
    # second number on bottom 13 bits except for the bottom 3, etc.)
    #
    # Therefore, if we know a value for A has the first "k"
    # output numbers correct, we trust the bottom 3*k bits and
    # play with the ten bits above that to find our next crop of
    # candidates
    progout = []
    avals = [0]
    correct = 0
    while progout != prog:
        navals = []
        for iaval in sorted(set(x % 8**correct for x in avals)):
            for nbit in range(1024):
                aval = iaval ^ (nbit * 8**correct)
                progout = runprog(aval)
                if progout == prog:
                    return aval
                if progout[: correct + 1] == prog[: correct + 1]:
                    # best = progout
                    # print("DBG", correct, aval, nbit, progout)
                    navals.append(aval)
        if not navals:
            break
        avals = sorted(navals)
        correct +=1
        # print("DBG: ", best[:correct+1], correct, len(avals), (min(avals), max(avals)), 8**aexp)
        # if correct == 7:
        #     print("avals: ", avals[:10])


print("Part 2:", part2())
