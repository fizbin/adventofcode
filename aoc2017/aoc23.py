import sys
import re
import collections

infile = "aoc23.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)


def isprime(n):
    d = 2
    while d * d <= n:
        if n % d == 0:
            return False
        d += 1
    return True


def do_step(ip, reg, ans):
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
    elif instr == "add":
        reg[stuff[1]] += reg_or_val(stuff[2])
    elif instr == "sub":
        reg[stuff[1]] -= reg_or_val(stuff[2])
    elif instr == "mul":
        reg[stuff[1]] *= reg_or_val(stuff[2])
        ans += 1
    elif instr == "nop":
        pass
    elif instr == "isprime":
        reg[stuff[1]] = int(isprime(reg_or_val(stuff[2])))
    elif instr == "addans":
        ans += reg_or_val(stuff[1])
    elif instr == "jnz":
        if reg_or_val(stuff[1]) != 0:
            ip += reg_or_val(stuff[2]) - 1
    ip += 1
    return (ip, instr, ans)


# part 1
reg = collections.defaultdict(lambda: 0)
ip = 0
ans = 0
while 0 <= ip < len(data):
    (new_ip, instr, new_ans) = do_step(ip, reg, ans)
    m = re.search(r"\bdbg(\w+)$", data[ip].strip())
    if m:
        print(
            f"Debug {m.group(1)}: {ip} => {new_ip} {dict(sorted(reg.items()))} ({ans})"
        )
        sys.stdout.flush()
    ip = new_ip
    ans = new_ans
print(ans)

# part 2


def findreplace(find: str, replace: str):
    find_list = find.strip().split("\n")
    replace_list = replace.strip().split("\n")
    while len(replace_list) < len(find_list):
        replace_list.append("\n")
    for idx, line in enumerate(data):
        if re.sub(r"#.*", "", line).strip() == re.sub(r"#.*", "", find_list[0]).strip():
            for findidx, findline in enumerate(find_list):
                if (
                    re.sub(r"#.*", "", data[idx + findidx]).strip()
                    != re.sub(r"#.*", "", findline).strip()
                ):
                    break
            else:
                for repidx, rep in enumerate(replace_list):
                    data[idx + repidx] = rep
                break
    else:
        raise Exception("Couldn't find spot to replace")


findreplace(
    """
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
    """,
    """
set f 0
set e b
    """,
)

findreplace(
    """
set e 2
set f 0
set e b







sub d -1
set g d
sub g b
jnz g -13
    """,
    """
               # set e 2 
               #                # set g d 
               #                # mul g e 
set e b        #                # sub g b
set d b        # addans b       # jnz g 2
set g b        # addans -2      # set f 0
sub g 4        # set f 0        # sub e -1 
mul g b        # set e b        # set g e
               #                # sub g b
               #                # jnz g -8  
isprime f b    # sub d -1  # dbgfoo #
               # set g d
               # sub g b
               # jnz g -13 # dbgfoo #
    """,
)

reg = collections.defaultdict(lambda: 0)
reg["a"] = 1
ip = 0
while 0 <= ip < len(data):
    (new_ip, instr, new_ans) = do_step(ip, reg, ans)
    m = re.search(r"\bdbg(\w+)$", data[ip].strip())
    if m:
        print(
            f"Debug {m.group(1)}: {ip} => {new_ip} {dict(sorted(reg.items()))} ({ans})"
        )
        sys.stdout.flush()
    ip = new_ip
print(reg["h"])
