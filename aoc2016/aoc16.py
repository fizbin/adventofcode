import sys

startseq = "01111001100111011"
if len(sys.argv) > 1:
    startseq = sys.argv[1]


def checksum(s):
    while len(s) % 2 == 0:
        c = ""
        for idx in range(0, len(s), 2):
            c += "1" if s[idx] == s[idx + 1] else "0"
        s = c
    return s


seq = startseq
while len(seq) < 272:
    seq = (
        seq
        + "0"
        + "".join(reversed(seq)).replace("0", "b").replace("1", "0").replace("b", "1")
    )
seq = seq[:272]
print(checksum(seq))

seq = startseq
while len(seq) < 35651584:
    seq = (
        seq
        + "0"
        + "".join(reversed(seq)).replace("0", "b").replace("1", "0").replace("b", "1")
    )
seq = seq[:35651584]
print(checksum(seq))
