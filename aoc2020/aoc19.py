import sys
import re

# each rule function is a function that takes
# "s" - the string under test
# "idx" - index to start matching this rule at
# and returns a list of the possible indexes you can be looking at
# once the rule matches. To check whether a whole string matches,
# you just see if len(s) is in the output of rule[0](s, 0)

# In retrospect, I probably would have done better with generic
# "two rules in sequence" and "two rules separated by |" functions,
# but I only covered what I saw in the input with my mk* functions.

# For rules that are just "a"
def mksimpl(char):
    def _matchrule(s, idx):
        if idx < len(s) and s[idx] == char:
            return [idx + 1]
        return []
    return _matchrule

# For the case NN: YY
def mksing(rules, fst):
    def _matchrule(s, idx):
        return rules[fst](s, idx)
    return _matchrule

# for the case NN: YY | ZZ
def mksing2(rules, fst, snd):
    def _matchrule(s, idx):
        return rules[fst](s, idx) + rules[snd](s, idx)
    return _matchrule

# for the case NN: YY ZZ
def mkdbl(rules, fst, snd):
    def _matchrule(s, idx):
        retval = []
        for i1 in rules[fst](s, idx):
            retval.extend(rules[snd](s, i1))
        return retval
    return _matchrule

# for the case NN: Y1 Z1 | Y2 Z2
def mkquad(rules, fst, snd, fst2, snd2):
    def _matchrule(s, idx):
        retval = []
        for i1 in rules[fst](s, idx):
            retval.extend(rules[snd](s, i1))
        for i2 in rules[fst2](s, idx):
            retval.extend(rules[snd2](s, i2))
        return retval
    return _matchrule

# in part 2:
# 8: 42 | 42 8
# 11: 42 31 | 42 11 31

def partTwoRule8(rules):
    # Wasted a decent amount of time here being clever so that I don't
    # recurse because the problem text warned me. I should have just
    # recursed like I did with part 2 rule 11.
    def _matchrule(s, idx):
        retval = set(rules[42](s, idx))
        retval1 = set()
        while retval1 != retval:
            retval1 = set(retval)
            for i1 in retval1:
                retval |= set(rules[42](s, i1))
        return sorted(retval)
    return _matchrule

def partTwoRule11(rules):
    def _matchrule(s, idx):
        initial = set(rules[42](s, idx))
        retval = set()
        for i1 in initial:
            retval |= set(rules[31](s, i1))
        partial = set()
        for i1 in initial:
            partial |= set(rules[11](s, i1))
        for i1 in partial:
            retval |= set(rules[31](s, i1))

        return sorted(retval)
    return _matchrule

def doit():
    with open('aoc19.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
        data = list(x.strip() for x in f)

    rules = {}
    tests = None
    for (idx, line) in enumerate(data):
        if not line:
            tests = data[idx+1:]
            break
        m = re.match(
            r'(\d+): *(?:"(.)"|(\d+ \d+)|(\d+ \d+ \| \d+ \d+)|(\d+)|(\d+ \| \d+)) *$',
            line)
        # print(line)
        rulenum = int(m.group(1))
        if m.group(2):
            rules[rulenum] = mksimpl(m.group(2))
        elif m.group(3):
            fst, snd = m.group(3).strip().split(' ')
            fst = int(fst)
            snd = int(snd)
            rules[rulenum] = mkdbl(rules, fst, snd)
        elif m.group(4):
            fst, snd, fst2, snd2 = re.findall(r'\d+', m.group(4))
            fst = int(fst)
            snd = int(snd)
            fst2 = int(fst2)
            snd2 = int(snd2)
            rules[rulenum] = mkquad(rules, fst, snd, fst2, snd2)
        elif m.group(5):
            rules[rulenum] = mksing(rules, int(m.group(5)))
        elif m.group(6):
            fst, snd = re.findall(r'\d+', m.group(6))
            rules[rulenum] = mksing2(rules, int(fst), int(snd))

    npass = 0
    for test in tests:
        npass += int(len(test) in rules[0](test, 0))
    print(npass)

    rules[8] = partTwoRule8(rules)
    rules[11] = partTwoRule11(rules)

    npass = 0
    for test in tests:
        npass += int(len(test) in rules[0](test, 0))
    print(npass)

if __name__ == '__main__':
    doit()
