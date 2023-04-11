import sys
import re
import collections

infile = "aoc18.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)


def do_step(ip, reg, inp_or_none):
    def reg_or_val(s):
        if re.fullmatch(r"-?\d+", s):
            return int(s)
        return reg.get(s)

    line = data[ip]
    stuff = line.split()
    instr = stuff[0]
    if instr == "snd":
        return (ip + 1, reg_or_val(stuff[1]), False)
    elif instr == "set":
        reg[stuff[1]] = reg_or_val(stuff[2])
    elif instr == "add":
        reg[stuff[1]] += reg_or_val(stuff[2])
    elif instr == "mul":
        reg[stuff[1]] *= reg_or_val(stuff[2])
    elif instr == "mod":
        reg[stuff[1]] %= reg_or_val(stuff[2])
    elif instr == "rcv":
        if inp_or_none is None:
            return (ip, None, True)
        reg[stuff[1]] = inp_or_none
    elif instr == "jgz":
        if reg_or_val(stuff[1]) > 0:
            ip += reg_or_val(stuff[2])
            return (ip, None, False)
    ip += 1
    return (ip, None, False)


# part 1
reg = collections.defaultdict(lambda: 0)
ip = 0
freq = 0
while 0 <= ip < len(data):
    (new_ip, snd_res, was_rcv) = do_step(ip, reg, None)
    if snd_res is not None:
        freq = snd_res
    if was_rcv:
        print(freq)
        break
    ip = new_ip

# part 2
reg = (collections.defaultdict(lambda: 0), collections.defaultdict(lambda: 0))
reg[0]["p"] = 0
reg[1]["p"] = 1
ip = [0, 0]
queue = ([], [])
waiting = [False, False]
sent_from_1 = 0
while True:
    for proggy in (0, 1):
        while 0 <= ip[proggy] < len(data):
            if waiting[proggy]:
                break
            (ip[proggy], snd_res, need_val) = do_step(ip[proggy], reg[proggy], None)
            if need_val:
                if queue[proggy]:
                    (ip[proggy], snd_res, need_val) = do_step(
                        ip[proggy], reg[proggy], queue[proggy].pop(0)
                    )
                else:
                    waiting[proggy] = True
                    break
            if snd_res is not None:
                queue[1 - proggy].append(snd_res)
                if proggy == 1:
                    sent_from_1 += 1
    for proggy in (0, 1):
        if waiting[proggy]:
            if queue[proggy]:
                waiting[proggy] = False
    if waiting[1] and not (0 <= ip[0] < len(data)):
        break
    if waiting[0] and not (0 <= ip[1] < len(data)):
        break
    if all(waiting):
        break
    # print(ip, waiting)

print(sent_from_1)
