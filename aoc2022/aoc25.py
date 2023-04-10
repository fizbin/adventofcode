from aoc_util import get_data_lines

data = get_data_lines(25)


def from_snafu(instr):
    (lastchr, rest) = (instr[-1], instr[:-1])
    if lastchr in "012":
        val = int(lastchr)
    elif lastchr == "=":
        val = -2
    elif lastchr == "-":
        val = -1
    else:
        assert False
    if rest:
        return val + 5 * from_snafu(rest)
    return val


def to_snafu(val):
    lastchr = {0: "0", 1: "1", 2: "2", 3: "=", 4: "-"}[val % 5]
    nval = val - from_snafu(lastchr)
    if nval:
        return to_snafu(nval // 5) + lastchr
    return lastchr


print(to_snafu(sum(from_snafu(ln) for ln in data)))
