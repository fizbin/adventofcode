import sys
import re
import functools

def mksimpl(char):
    @functools.lru_cache(maxsize=None)    
    def _genrule(test_str):
        if test_str and test_str.startswith(char):
            return set([char])
        return []
    return _genrule

def mkgenrule(rules, nums):
    # nums is a list of list of other rules to call to;
    # e.g. "1 2 | 3 4" means nums is [[1,2],[3,4]]
    @functools.lru_cache(maxsize=None)
    def _genrule(test_str):
        if not test_str:
            return set()
        retval = set()
        for ruleseq in nums:
            sofar = set([''])
            for rule in ruleseq:
                sofar = set(so + new
                            for so in sofar
                            for new in rules[rule](test_str[len(so):]))
            retval |= sofar
        return retval
    return _genrule

# in part 2:
# 8: 42 | 42 8
# 11: 42 31 | 42 11 31

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
            r'(\d+): *(?:"(.)"|(\d.*\S|\d)) *$',
            line)
        # print(line)
        rulenum = int(m.group(1))
        if m.group(2):
            rules[rulenum] = mksimpl(m.group(2))
        elif m.group(3):
            ruleseqs = m.group(3).split(' | ')
            rulenums = [list(map(int, s.split(' '))) for s in ruleseqs]
            rules[rulenum] = mkgenrule(rules, rulenums)

    npass = 0
    for test in tests:
        npass += int(test in rules[0](test))
    print(npass)

    rules[8] = mkgenrule(rules, [[42], [42, 8]])
    rules[11] = mkgenrule(rules, [[42, 31], [42, 11, 31]])
    for rule in rules.values():
        rule.cache_clear()

    npass = 0
    for test in tests:
        npass += int(test in rules[0](test))
    print(npass)

if __name__ == '__main__':
    doit()
