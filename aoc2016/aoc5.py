import sys
import hashlib


inputstr = "ffykfhsq"
if len(sys.argv) > 1:
    inputstr = sys.argv[1]

input = inputstr.encode("ascii")
retval = ""
counter = 0
while len(retval) < 8:
    while (
        not hashlib.md5(input + str(counter).encode("ascii"))
        .hexdigest()
        .startswith("00000")
    ):
        counter += 1
    retval += hashlib.md5(input + str(counter).encode("ascii")).hexdigest()[5]
    counter += 1

print(retval)

retval = ["_"] * 8
counter = 0
while "_" in retval:
    while (
        not hashlib.md5(input + str(counter).encode("ascii"))
        .hexdigest()
        .startswith("00000")
    ):
        counter += 1
    digest = hashlib.md5(input + str(counter).encode("ascii")).hexdigest()
    if digest[5].isalpha() or int(digest[5]) > 7:
        pass
    elif retval[int(digest[5])] == "_":
        retval[int(digest[5])] = digest[6]
    print("", "".join(retval), f"({digest[6]})", end="\r")
    counter += 1

print()
print("".join(retval))
