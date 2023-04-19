import sys
import re


def do_step(ip, reg):
    def reg_or_val(s):
        if re.fullmatch(r"-?\d+", s):
            return int(s)
        return reg[s]

    line = data[ip]
    line = re.sub(r"#.*", "", line)
    stuff = line.split()
    if line.strip():
        instr = stuff[0]
    else:
        instr = "nop"
    if instr == "set":
        reg[stuff[1]] = reg_or_val(stuff[2])
    elif instr == "cpy":
        reg[stuff[2]] = reg_or_val(stuff[1])
    elif instr == "add":
        reg[stuff[1]] += reg_or_val(stuff[2])
    elif instr == "sub":
        reg[stuff[1]] -= reg_or_val(stuff[2])
    elif instr == "inc":
        reg[stuff[1]] += 1
    elif instr == "dec":
        reg[stuff[1]] -= 1
    elif instr == "mul":
        reg[stuff[1]] *= reg_or_val(stuff[2])
        ans += 1
    elif instr == "nop":
        pass
    elif instr == "jnz":
        if reg_or_val(stuff[1]) != 0:
            ip += reg_or_val(stuff[2]) - 1
    ip += 1
    return (ip, instr)


infile = "aoc12.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    datawhole = infilep.read()


datawhole = re.sub(
    r"inc (.)\s*\ndec (.)\s*\njnz \2 -2", r"add \1 \2\ncpy 0 \2\nnop", datawhole
)
datawhole = re.sub(
    r"dec (.)\s*\ninc (.)\s*\njnz \1 -2", r"add \2 \1\ncpy 0 \1\nnop", datawhole
)
data = datawhole.splitlines()

# print(datawhole)


reg = {"a": 0, "b": 0, "c": 0, "d": 0}
ip = 0
while 0 <= ip < len(data):
    preg = dict(reg)
    (new_ip, instr) = do_step(ip, reg)
    # if instr == 'jnz':
    #     print(ip, repr(preg), repr(data[ip]), new_ip, repr(reg))
    ip = new_ip
print(reg["a"])

reg = {"a": 0, "b": 0, "c": 1, "d": 0}
ip = 0
while 0 <= ip < len(data):
    preg = dict(reg)
    (new_ip, instr) = do_step(ip, reg)
    # if instr == 'jnz':
    #     print(ip, repr(preg), repr(data[ip]), new_ip, repr(reg))
    ip = new_ip
print(reg["a"])
