#!/usr/bin/python

import aoc_util
import copy
import re

(rules, partspec) = aoc_util.get_data_paras(19)


def parse_rule1(rule):
    if "," not in rule:
        return lambda _: rule
    (mybit, rest) = rule.split(",", 1)
    restrule = parse_rule1(rest)
    m1 = re.match(r"(\w)(<|>)(\d+):(\w+)", mybit)
    attr = m1.group(1)
    dec = m1.group(2)
    val = int(m1.group(3))
    ans = m1.group(4)

    def ret_lt(part):
        if part[attr] < val:
            return ans
        return restrule(part)

    def ret_gt(part):
        if part[attr] > val:
            return ans
        return restrule(part)

    if dec == ">":
        return ret_gt
    return ret_lt


ruledict = {}
for rule in rules.splitlines():
    m2 = re.match(r"(\w+)\{([^{}]*)\}", rule)
    ruledict[m2.group(1)] = parse_rule1(m2.group(2))

parts = []
for partstr in partspec.splitlines():
    m2 = re.match(r"{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}", partstr)
    part = {
        "x": int(m2.group(1)),
        "m": int(m2.group(2)),
        "a": int(m2.group(3)),
        "s": int(m2.group(4)),
    }
    parts.append(part)

accepted = []
for part in parts:
    flow = "in"
    while flow not in ("A", "R"):
        flow1 = ruledict[flow](part)
        flow = flow1
    if flow == "A":
        accepted.append(part)
print(sum(sum(v.values()) for v in accepted))


# range convention is usual python convention: (a, b) means a <= x < b


def parse_rule2(rule):
    if "," not in rule:
        return lambda part: [(part, rule)]
    (mybit, rest) = rule.split(",", 1)
    restrule = parse_rule2(rest)
    m1 = re.match(r"(\w)(<|>)(\d+):(\w+)", mybit)
    attr = m1.group(1)
    dec = m1.group(2)
    val = int(m1.group(3))
    ans = m1.group(4)

    def ret_lt(part):
        if part[attr][1] <= val:
            return [(part, ans)]
        elif part[attr][0] >= val:
            return restrule(part)
        # else split the part
        p1 = copy.copy(part)
        p2 = copy.copy(part)
        p1[attr] = (part[attr][0], val)
        p2[attr] = (val, part[attr][1])
        return [(p1, ans)] + restrule(p2)

    def ret_gt(part):
        if part[attr][0] >= val + 1:
            return [(part, ans)]
        elif part[attr][1] <= val + 1:
            return restrule(part)
        p1 = copy.copy(part)
        p2 = copy.copy(part)
        p1[attr] = (part[attr][0], val + 1)
        p2[attr] = (val + 1, part[attr][1])
        return [(p2, ans)] + restrule(p1)

    if dec == ">":
        return ret_gt
    return ret_lt


ruledict = {}
for rule in rules.splitlines():
    m2 = re.match(r"(\w+)\{([^{}]*)\}", rule)
    ruledict[m2.group(1)] = parse_rule2(m2.group(2))

accepted = []
working = [("in", {"x": (1, 4001), "m": (1, 4001), "a": (1, 4001), "s": (1, 4001)})]
while working:
    nworking = []
    for flow, part in working:
        for npart, nflow in ruledict[flow](part):
            if nflow == "A":
                accepted.append(npart)
            elif nflow == "R":
                pass
            else:
                nworking.append((nflow, npart))
    working = nworking


def poss(partish):
    retval = partish["x"][1] - partish["x"][0]
    retval *= partish["m"][1] - partish["m"][0]
    retval *= partish["a"][1] - partish["a"][0]
    retval *= partish["s"][1] - partish["s"][0]
    return retval


print(sum(poss(a) for a in accepted))
