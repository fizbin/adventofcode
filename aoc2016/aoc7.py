import sys
import re


infile = "aoc7.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = [x.strip() for x in infilep]

retval = 0
for line in data:
    if re.search(r"(.)((?!\1).)\2\1", re.sub(r"\[[^]]*\]", " ", line)):
        if re.search(r"(.)((?!\1).)\2\1", re.sub(r"(\]|^)[^[]*(\[|$)", " ", line)):
            continue
        retval += 1
print(retval)

retval = 0
for line in data:
    out_of_brackets = re.sub(r"\[[^]]*\]", ";", line)
    in_brackets = re.sub(r"(\]|^)[^[]*(\[|$)", ";", line)
    for m in re.finditer(r"(\w)(?!\1)(?=.\1)", out_of_brackets):
        letter_a = m.group(1)
        letter_b = out_of_brackets[m.end(1)]
        if f"{letter_b}{letter_a}{letter_b}" in in_brackets:
            retval += 1
            break

print(retval)
