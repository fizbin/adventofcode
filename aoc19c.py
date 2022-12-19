from aoc_util import *
import copy
import itertools
from functools import reduce
import re
import heapq

# under the situation of infinite ore, what's the max geode?
def pot_geo_total(time, robos, rocks, blueprint, maxt):
    (_a, crocks, orocks, grocks) = rocks
    (_a, crobos, orobos, grobos) = robos
    # if time == 8 and sum(robos) == 1:
    #     breakpoint()
    for tick in range(time, maxt):
        grocks += grobos
        orocks += orobos
        crocks += crobos
        if orocks - orobos >= blueprint[3][2]:
            orocks -= blueprint[3][2]
            grobos += 1
        if crocks - crobos >= blueprint[2][1]:
            crocks -= blueprint[2][1]
            orobos += 1
        crobos += 1
    return grocks

def prune_state(tick, state, blueprint, maxt):
    max_do_nothing = max((maxt - tick)*st[0][3] + st[1][3] for st in state)
    return set(st for st in state if pot_geo_total(tick, st[0], st[1], blueprint, maxt) >= max_do_nothing)

# ore, clay, obsidian, geode
def do_minute(blueprint, tick, state):
    retval1 = set()
    for (robos, rocks) in state:
        # factory does nothing
        nextrocks = (rocks[0] + robos[0], rocks[1] + robos[1], rocks[2] + robos[2], rocks[3] + robos[3])
        retval1.add((robos, nextrocks))
        # factory does something
        for fac_action in range(4):
#            print(repr(blueprint))
            if any(blueprint[fac_action][i] > rocks[i] for i in range(4)):
                continue
            lrobos = list(robos)
            lnrocks = list(nextrocks)
            lrobos[fac_action] += 1
            for i in range(4):
                lnrocks[i] -= blueprint[fac_action][i]
            if any(r < 0 for r in lnrocks):
                breakpoint()
            retval1.add((tuple(lrobos), tuple(lnrocks)))
    return retval1

# ore, clay, obsidian, geode
def parse_blueprint(linestr):
    line = numbers(linestr)
    return (line[0], ((line[1], 0, 0, 0), (line[2], 0, 0, 0), (line[3], line[4], 0, 0), (line[5], 0, line[6], 0)))

data = get_data_lines(19)

# data = '''
# Blueprint 1:
#   Each ore robot costs 4 ore.
#   Each clay robot costs 2 ore.
#   Each obsidian robot costs 3 ore and 14 clay.
#   Each geode robot costs 2 ore and 7 obsidian.

# Blueprint 2:
#   Each ore robot costs 2 ore.
#   Each clay robot costs 3 ore.
#   Each obsidian robot costs 3 ore and 8 clay.
#   Each geode robot costs 3 ore and 12 obsidian.'''.split('\n\n')

blueprints = [parse_blueprint(line) for line in data]

results = {}
for blueprint in blueprints:
    state = set([((1,0,0,0),(0,0,0,0))])
    for tick in range(24):
        prune_state(tick, state, blueprint[1], 24)
        state = do_minute(blueprint[1], tick, state)
        print(blueprint[0], tick, len(state)) # , sorted(state, key=lambda s:s[1][3])[-1])
    results[blueprint[0]] = max(poss[1][3] for poss in state)

tot = 0
for (n, geo) in results.items():
    print(n,geo)
    tot += n * geo
print(tot)
