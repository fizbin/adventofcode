from aoc_util import *
import copy
import itertools
from functools import reduce
import re
import heapq

# under the situation of infinite ore, what's the max geode?
def geo_overstimate(timeleft, robos, rocks, blueprint):
    (_a, crocks, orocks, grocks) = rocks
    (_a, crobos, orobos, grobos) = robos
    # if time == 8 and sum(robos) == 1:
    #     breakpoint()
    for _ in range(timeleft):
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

def geo_underestimate(timeleft, robos, rocks, blueprint):
    return timeleft * robos[3] + rocks[3]

def prune_state(tick, state, blueprint, maxt):
    max_do_nothing = max((maxt - tick)*st[0][3] + st[1][3] for st in state)
    return set(st for st in state if geo_overstimate(maxt - tick, st[0], st[1], blueprint) >= max_do_nothing)

def rocks_le(rocks1, rocks2):
    return all(rocks1[i] <= rocks2[i] for i in range(4))

# ore, clay, obsidian, geode
def do_minute(blueprint, timeleft, state, fired_opt_tracker):
    robomaxb = [0,0,0,0]
    for oretype in range(4):
        orereq = max(blueprint[i][oretype] for i in range(4))
        robomaxb[oretype] = orereq
    robomaxb[3] = 999
    robomax = tuple(robomaxb)
    retval1 = set()
    seenrocks = {}
    min_geo_overestimate = 1
    for (robos, rocks) in state:
        # factory does nothing
        nextrocks = (rocks[0] + robos[0], rocks[1] + robos[1], rocks[2] + robos[2], rocks[3] + robos[3])
        min_geo_overestimate = max(min_geo_overestimate, geo_underestimate(timeleft, robos, rocks, blueprint))
        if robos in seenrocks:
            if not rocks_le(rocks, seenrocks[robos]):
                seenrocks[robos] = rocks
        else:
            seenrocks[robos] = rocks
        retval1.add((robos, nextrocks))
    for (robos, rocks) in state:
        nextrocks = (rocks[0] + robos[0], rocks[1] + robos[1], rocks[2] + robos[2], rocks[3] + robos[3])
        # factory does something
        for fac_action in range(4):
#            print(repr(blueprint))
            if any(blueprint[fac_action][i] > rocks[i] for i in range(4)):
                continue
            if robos[fac_action] == robomax[fac_action]:
                continue
            lrobos = list(robos)
            lnrocks = list(nextrocks)
            lrobos[fac_action] += 1
            for i in range(4):
                lnrocks[i] -= blueprint[fac_action][i]
            if any(r < 0 for r in lnrocks):
                breakpoint()
            if geo_overstimate(timeleft, lrobos, lnrocks, blueprint) >= min_geo_overestimate:
                lrobost = tuple(lrobos)
                if lrobost in seenrocks and rocks_le(lnrocks, seenrocks[lrobost]):
                    fired_opt_tracker[0] += 1
                    continue
                retval1.add((lrobost, tuple(lnrocks)))
                min_geo_overestimate = max(min_geo_overestimate, geo_underestimate(timeleft, lrobos, lnrocks, blueprint))
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
    max_state_sz = 1
    fired_opt_tracker = [0]
    for tick in range(24):
        state = do_minute(blueprint[1], 23 - tick, state, fired_opt_tracker)
        max_state_sz = max(max_state_sz, len(state))
        # print(blueprint[0], tick, len(state)) # , sorted(state, key=lambda s:s[1][3])[-1])
    results[blueprint[0]] = max(poss[1][3] for poss in state)
    print(f"For {blueprint} got {results[blueprint[0]]} with at most {max_state_sz} states, rocks_le opt fired {fired_opt_tracker[0]} times")

tot = 0
for (n, geo) in results.items():
    # print(n,geo)
    tot += n * geo
print(tot)

results = {}
for blueprint in blueprints[0:3]:
    state = set([((1,0,0,0),(0,0,0,0))])
    for tick in range(32):
        state = do_minute(blueprint[1], 31 - tick, state, [0])
        print(blueprint[0], tick, len(state)) # , sorted(state, key=lambda s:s[1][3])[-1])
    results[blueprint[0]] = max(poss[1][3] for poss in state)
    print(f"For {blueprint} got {results[blueprint[0]]}")

tot = 1
for (n, geo) in results.items():
    tot *= geo
print(tot)
