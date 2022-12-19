from aoc_util import *
import copy
import itertools
from functools import reduce
import re
import heapq

# under the situation of infinite ore, what's the max geode?
def pot_geo_total(time, robos, rocks, blueprint, maxt):
    (_, crocks, orocks, grocks) = rocks
    (_, crobos, orobos, grobos) = robos
    # if time == 8 and sum(robos) == 1:
    #     breakpoint()
    for tick in range(time, maxt):
        grocks += grobos
        orocks += orobos
        crocks += crobos
        # Yeah, I should only be making one robot here, not potentially 3
        # so the result is an overestimate. That's fine.
        if orocks - orobos >= blueprint[3][2]:
            orocks -= blueprint[3][2]
            grobos += 1
        if crocks - crobos >= blueprint[2][1]:
            crocks -= blueprint[2][1]
            orobos += 1
        crobos += 1
    return grocks


# ore, clay, obsidian, geode
def do_run(blueprintn, blueprint, maxt):
    # heap is full of (sortkey, time, robots, rocks)
    # After the initial element, sortkey is -(potential_geodes), where
    # potential_geodes is as calculated by pot_geo_total
    hp = [(0, 0, (1, 0, 0, 0), (0, 0, 0, 0))]
    while hp:
        (npot, time, robos, rocks) = heapq.heappop(hp)
        # print(blueprintn, time, npot)
        if time == maxt:
            return rocks[3]
        nextrocks = (
            rocks[0] + robos[0],
            rocks[1] + robos[1],
            rocks[2] + robos[2],
            rocks[3] + robos[3],
        )
        # do nothing
        pot = pot_geo_total(time + 1, robos, nextrocks, blueprint, maxt)
        if pot > 0:
            heapq.heappush(hp, (-pot, time + 1, robos, nextrocks))
        for fac_action in range(4):
            if any(blueprint[fac_action][i] > rocks[i] for i in range(4)):
                continue
            lrobos = list(robos)
            lnrocks = list(nextrocks)
            for i in range(4):
                lnrocks[i] -= blueprint[fac_action][i]
            lrobos[fac_action] += 1
            pot = pot_geo_total(time + 1, lrobos, lnrocks, blueprint, maxt)
            if pot > 0:
                heapq.heappush(hp, (-pot, time + 1, tuple(lrobos), tuple(lnrocks)))
    return 0


# ore, clay, obsidian, geode
def parse_blueprint(linestr):
    line = numbers(linestr)
    return (
        line[0],
        (
            (line[1], 0, 0, 0),
            (line[2], 0, 0, 0),
            (line[3], line[4], 0, 0),
            (line[5], 0, line[6], 0),
        ),
    )


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
    results[blueprint[0]] = do_run(blueprint[0], blueprint[1], 24)
    # print(">", blueprint[0], results[blueprint[0]])

tot = 0
for (n, geo) in results.items():
    tot += n * geo
print(tot)

blueprints = [parse_blueprint(line) for line in data]
blueprints = blueprints[0:3]

results = {}
for blueprint in blueprints:
    results[blueprint[0]] = do_run(blueprint[0], blueprint[1], 32)
    # print(">", blueprint[0], results[blueprint[0]])

tot = 1
for (n, geo) in results.items():
    tot *= geo
print(tot)
