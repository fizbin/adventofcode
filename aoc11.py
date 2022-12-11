from aoc_util import get_data_paras, numbers
from functools import reduce
import copy

monkeyspecs = get_data_paras(11)
monkeys = []
for spec in monkeyspecs:
    lines = spec.splitlines()
    (monkeynum,) = numbers(lines[0])
    mitems = numbers(lines[1])
    assert lines[2].startswith("  Operation: new = ")
    (_, rst) = lines[2].split("=")
    bump = eval("lambda old: " + rst)
    (testdiv,) = numbers(lines[3])
    (throwt,) = numbers(lines[4])
    (throwf,) = numbers(lines[5])
    monkeys.append(
        {
            "num": monkeynum,
            "it": list(mitems),
            "bump": bump,
            "tdiv": testdiv,
            "t": throwt,
            "f": throwf,
        }
    )
og_monkeys = copy.deepcopy(monkeys)

inspectioncounts = {mk["num"]: 0 for mk in monkeys}
for roundnum in range(1, 21):
    for monkeynum in range(len(monkeys)):
        monkey = monkeys[monkeynum]
        items = list(monkey["it"])
        monkey["it"] = []
        for item in items:
            inspectioncounts[monkeynum] += 1
            item = monkey["bump"](item)
            item //= 3
            if item % monkey["tdiv"] == 0:
                nmonk = monkey["t"]
            else:
                nmonk = monkey["f"]
            monkeys[nmonk]["it"].append(item)

x = sorted(inspectioncounts.values())
print(x[-1] * x[-2])

monkeys = og_monkeys

bigmod = reduce(lambda x, y: x * y, [mk["tdiv"] for mk in monkeys])
inspectioncounts = {mk["num"]: 0 for mk in monkeys}
for roundnum in range(1, 10001):
    for monkeynum in range(len(monkeys)):
        monkey = monkeys[monkeynum]
        items = list(monkey["it"])
        monkey["it"] = []
        for item in items:
            inspectioncounts[monkeynum] += 1
            item = monkey["bump"](item) % bigmod
            if item % monkey["tdiv"] == 0:
                nmonk = monkey["t"]
            else:
                nmonk = monkey["f"]
            monkeys[nmonk]["it"].append(item)
    # if roundnum in (1, 20, 1000, 2000):
    #     print(roundnum)
    #     print(inspectioncounts)

x = sorted(inspectioncounts.values())
print(x[-1] * x[-2])
