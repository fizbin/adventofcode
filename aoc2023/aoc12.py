#!/usr/bin/python

import aoc_util
import re
import itertools
from functools import cache


def handle_line1(line: str):
    (fixrecord, seqnumstr) = line.split(" ")
    seqnums = [int(s) for s in seqnumstr.split(",")]
    validateregex = re.compile("^.*" + "[.]+".join("#" * s for s in seqnums) + "[.]*$")
    numpounds = len([x for x in fixrecord if x == "#"])
    questionspots = [idx for (idx, ch) in enumerate(fixrecord) if ch == "?"]
    totseqnums = sum(seqnums)
    neededpounds = totseqnums - numpounds
    retval = 0
    alldots = fixrecord.replace("?", ".")
    if neededpounds >= 0:
        for combo in itertools.combinations(questionspots, neededpounds):
            test = list(alldots)
            for i in combo:
                test[i] = "#"
            if validateregex.match("".join(test)):
                retval += 1
    # print("DBG", line, ":", retval)
    return retval


def embiggen(line):
    (fixrecord, seqnumstr) = line.split(" ")
    return f"{'?'.join([fixrecord]*5)} {','.join([seqnumstr]*5)}"


def handle_line2(line: str):
    (fixrecord, seqnumstr) = line.split(" ")
    seqnums = [int(s) for s in seqnumstr.split(",")]

    @cache
    def combos(str_rem, seq_rem):
        if not seq_rem:
            if "#" in str_rem:
                return 0
            return 1
        if not str_rem:
            return 0
        retval = 0
        if str_rem[0] in ".?":
            retval += combos(str_rem[1:], seq_rem)
        if str_rem[0] in "?#":
            if len(str_rem) < seq_rem[0] or "." in str_rem[0 : seq_rem[0]]:
                retval += 0
            else:
                # phrased this way to catch also the case where len(str_rem) == seq_rem[0]
                if str_rem[seq_rem[0] : seq_rem[0] + 1] != "#":
                    retval += combos(str_rem[seq_rem[0] + 1 :], seq_rem[1:])
                else:
                    retval += 0
        return retval

    rval = combos(fixrecord, tuple(seqnums))
    # print("DBG", line, ":", rval)
    return rval


data = aoc_util.get_data_lines(12)
print(sum(handle_line1(ln) for ln in data))
print(sum(handle_line2(embiggen(ln)) for ln in data))
