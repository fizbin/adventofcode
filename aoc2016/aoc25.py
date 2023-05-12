import sys
import re
import collections

infile = "aoc25.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

dbg_ip = collections.Counter()

mul_opt_re = re.compile(
    r"cpy (?P<b>\S+) (?P<c>.)\s+inc (?P<a>.)\s+dec (?P=c)\s+jnz (?P=c) -2\s+dec (?P<d>.)\s+jnz (?P=d) -5\s+"
)


def do_step(ip, reg):
    def reg_or_val(s):
        if re.fullmatch(r"-?\d+", s):
            return int(s)
        return reg[s]

    line = data[ip]
    line = re.sub(r"#.*", "", line)
    stuff = line.split()
    out = None
    dbg_ip.update([ip])
    if dbg_ip.total() == 10000:
        # print("dbg_ip", dbg_ip.most_common())
        dbg_ip.clear()
    for _ in range(1):
        if line.strip():
            instr = stuff[0]
        else:
            instr = "nop"
        if instr == "set":
            reg[stuff[1]] = reg_or_val(stuff[2])
        elif instr == "cpy":
            if ip < len(data) - 5:
                if m := mul_opt_re.fullmatch("".join(data[ip : ip + 6])):
                    reg[m.group("a")] += reg_or_val(m.group("b")) * reg[m.group("d")]
                    reg[m.group("c")] = 0
                    reg[m.group("d")] = 0
                    ip += 5
                    continue
            reg[stuff[2]] = reg_or_val(stuff[1])
        elif instr == "add":
            reg[stuff[1]] += reg_or_val(stuff[2])
        elif instr == "sub":
            reg[stuff[1]] -= reg_or_val(stuff[2])
        elif instr == "inc":
            # can we speed this up?
            if (
                ip < len(data) - 2
                and data[ip + 1].startswith("dec")
                and data[ip + 2].startswith("jnz")
                and data[ip + 2].strip().endswith(" -2")
            ):
                stuff1 = data[ip + 1].split()
                stuff2 = data[ip + 2].split()
                if stuff1[1] == stuff2[1]:
                    reg[stuff[1]] += reg[stuff1[1]]
                    reg[stuff1[1]] = 0
                    ip += 2
                    # print("opt1")
                    continue
            reg[stuff[1]] += 1
        elif instr == "dec":
            if (
                ip < len(data) - 2
                and data[ip + 1].startswith("inc")
                and data[ip + 2].startswith("jnz")
                and data[ip + 2].strip().endswith(" -2")
            ):
                stuff1 = data[ip + 1].split()
                stuff2 = data[ip + 2].split()
                if stuff[1] == stuff2[1]:
                    reg[stuff1[1]] += reg[stuff[1]]
                    reg[stuff[1]] = 0
                    ip += 2
                    # print("opt2")
                    continue
            reg[stuff[1]] -= 1
        elif instr == "mul":
            reg[stuff[1]] *= reg_or_val(stuff[2])
            ans += 1
        elif instr == "nop":
            pass
        elif instr == "jnz":
            if reg_or_val(stuff[1]) != 0:
                ip += reg_or_val(stuff[2]) - 1
        elif instr == "tgl":
            oip = ip + reg_or_val(stuff[1])
            if 0 <= oip < len(data):
                oline = data[oip]
                if m := re.match(r"(\S+)\s+(\S+)\s*$", oline):
                    oline = f"{'dec' if 'inc' == m.group(1) else 'inc'} {m.group(2)}\n"
                elif m := re.match(r"(\S+)\s+(\S+\s+\S+)\s*$", oline):
                    oline = f"{'cpy' if 'jnz' == m.group(1) else 'jnz'} {m.group(2)}\n"
                data[oip] = oline
                # print(f"{oip} became {oline.strip()}")
        elif instr == 'out':
            out = reg_or_val(stuff[1])
        else:
            raise Exception(f"Unknown line at {ip}: {line}")
    ip += 1
    return (ip, out)

starta = 0
done = False
while not done:
    starta += 1
    expected_out = 0
    outn = 0
    reg = {"a": starta, "b": 0, "c": 0, "d": 0}
    ip = 0
    been_there = set()
    dbg_ip.clear()
    print(f"Trying {starta}")
    while 0 <= ip < len(data):
        if (ip, tuple(reg.items())) in been_there:
            if outn > 1:
                done = True
            break
        been_there.add((ip, tuple(reg.items())))
        (new_ip, out) = do_step(ip, reg)
        if out is not None:
            if out == expected_out:
                expected_out = 1 - expected_out
                outn += 1
            else:
                outn = 0
                break
        ip = new_ip

print(starta)
