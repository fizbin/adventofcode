import sys
import re
from collections import defaultdict

with open('aoc7.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

signals = {}
alarms = defaultdict(set)

newsignals = set([])

for ln in data:
    def do_lshift(oper, amt, out):
        def it():
            signals[out] = 65535 & (signals[oper] << amt)
            newsignals.add(out)
        return it
    def do_rshift(oper, amt, out):
        def it():
            signals[out] = 65535 & (signals[oper] >> amt)
            newsignals.add(out)
        return it
    def do_not(oper, out):
        def it():
            signals[out] = 65535 & ~signals[oper]
            newsignals.add(out)
        return it
    def do_and(oper1, oper2, out):
        def it():
            if oper1 in signals and oper2 in signals:
                signals[out] = signals[oper1] & signals[oper2]
                newsignals.add(out)
        return it
    def do_or(oper1, oper2, out):
        def it():
            if oper1 in signals and oper2 in signals:
                signals[out] = signals[oper1] | signals[oper2]
                newsignals.add(out)
        return it
    def do_assign(oper1, out):
        def it():
            signals[out] = signals[oper1]
            newsignals.add(out)
        return it

    m = re.match(r'(\d+) -> ([a-z]+)', ln)
    if m:
        signals[m.group(2)] = int(m.group(1))
        newsignals.add(m.group(2))
        continue
    m = re.match(r'([a-z]+) -> ([a-z]+)', ln)
    if m:
        act = do_assign(m.group(1), m.group(2))
        alarms[m.group(1)].add(act)
        continue
    m = re.match(r'([a-z]+) AND ([a-z]+) -> ([a-z]+)', ln)
    if m:
        act = do_and(m.group(1), m.group(2), m.group(3))
        alarms[m.group(1)].add(act)
        alarms[m.group(2)].add(act)
        continue
    m = re.match(r'([a-z]+) OR ([a-z]+) -> ([a-z]+)', ln)
    if m:
        act = do_or(m.group(1), m.group(2), m.group(3))
        alarms[m.group(1)].add(act)
        alarms[m.group(2)].add(act)
        continue
    m = re.match(r'([a-z]+) LSHIFT (\d+) -> ([a-z]+)', ln)
    if m:
        act = do_lshift(m.group(1), int(m.group(2)), m.group(3))
        alarms[m.group(1)].add(act)
        continue
    m = re.match(r'([a-z]+) RSHIFT (\d+) -> ([a-z]+)', ln)
    if m:
        act = do_rshift(m.group(1), int(m.group(2)), m.group(3))
        alarms[m.group(1)].add(act)
        continue
    m = re.match(r'NOT ([a-z]+) -> ([a-z]+)', ln)
    if m:
        act = do_not(m.group(1), m.group(2))
        alarms[m.group(1)].add(act)
        continue
    m = re.match(r'(\d+) AND ([a-z]+) -> ([a-z]+)', ln)
    if m:
        fakename = 'fake%d' % int(m.group(1))
        signals[fakename] = int(m.group(1))
        newsignals.add(fakename)
        act = do_and(fakename, m.group(2), m.group(3))
        alarms[fakename].add(act)
        alarms[m.group(2)].add(act)
        continue
    m = re.match(r'(\d+) OR ([a-z]+) -> ([a-z]+)', ln)
    if m:
        fakename = 'fake%d' % int(m.group(1))
        signals[fakename] = int(m.group(1))
        newsignals.add(fakename)
        act = do_or(fakename, m.group(2), m.group(3))
        alarms[fakename].add(act)
        alarms[m.group(2)].add(act)
        continue
    raise Exception(repr(ln))

while newsignals:
    oldsigs = set(newsignals)
    newsignals.clear()
    for act in (a for s in oldsigs for a in alarms[s]):
        act()

print(signals['a'])
