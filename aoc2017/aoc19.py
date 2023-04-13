import sys

infile = "aoc19.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

def char_at(cpx):
    return data[int(cpx.real)][int(cpx.imag)]

dcn = 1.0+0j
pos = 0+0j
while char_at(pos) == ' ':
    pos += 0+1j

retval = ''
steps = 1
while char_at(pos+dcn) != ' ':
    pos += dcn
    m = char_at(pos)
    if m == '+':
        if char_at(pos+(dcn*1j)) != ' ':
            dcn *= 1j
        else:
            dcn *= -1j
    elif m.isalpha():
        retval += m
    steps += 1

print(retval)
print(steps)
