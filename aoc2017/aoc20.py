import sys
import re
import math
import numpy as np
import itertools

infile = "aoc20.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

particles = []
for line in data:
    m = re.match('p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>', line)
    particles.append({"p": np.array([int(m.group(1)), int(m.group(2)),int(m.group(3))], dtype=int),
                      "v": np.array([int(m.group(4)), int(m.group(5)),int(m.group(6))], dtype=int),
                      "a": np.array([int(m.group(7)), int(m.group(8)),int(m.group(9))], dtype=int),
                      })

def manhattan(narr):
    return abs(narr).sum()

mv = None
mv_at = None
for (idx, particle) in enumerate(particles):
    man = manhattan(particle['a'])
    if mv is None or man < mv:
        mv_at = idx
        mv = man
print(mv_at)

def get_collision_times(particle_a, particle_b):
    p = particle_a['p'] - particle_b['p']
    v = particle_a['v'] - particle_b['v']
    a = particle_a['a'] - particle_b['a']
    
    # Solving 2*p + 2*t*v + (t^2 + t)*a == 0
    #         a*t^2 + (a + 2*v) t + 2*p == 0

    discrim = (a + 2*v) ** 2 - 8 * a * p
    if (discrim < 0).any():
        return []
    possible_t = None
    for idx in (0, 1, 2):
        if a[idx] != 0:
            isqrt = int(math.sqrt(discrim[idx]))
            if discrim[idx] != isqrt**2:
                return []
            val1 = (-a-2*v)[idx] + isqrt
            val2 = (-a-2*v)[idx] - isqrt
            my_t = set()
            if val1 % (2*a[idx]) == 0:
                my_t.add(val1 // (2*a[idx]))
            if val2 % (2*a[idx]) == 0:
                my_t.add(val2 // (2*a[idx]))
        else:
            if v[idx] == 0:
                if p[idx] != 0:
                    return []
                my_t = possible_t
            else:
                if p[idx] % v[idx] != 0:
                    return []
                my_t = set([p[idx] // v[idx]])
        if possible_t is None:
            possible_t = my_t
        else:
            possible_t = possible_t & my_t
    return sorted(possible_t)

def at_time(particle, t):
    return tuple(particle['p'] + t*particle['v'] + ((t*(t+1))//2)*particle['a'])

particle_bag = dict(enumerate(particles))

for collision_time in range(40):
    if collision_time < 0:
        continue
    pos_bag = dict((k, at_time(v, collision_time)) for (k, v) in particle_bag.items())
    re_org_pos = {}
    for (k, v) in pos_bag.items():
        re_org_pos.setdefault(v, []).append(k)
    eliminated = set()
    for (k, v) in re_org_pos.items():
        if len(v) > 1:
            eliminated.update(v)
            for idx in v:
                del particle_bag[idx]
    # print("debug:", collision_time, len(particle_bag), "eliminated", sorted(eliminated))


collision_times = set()
for (particle_a, particle_b) in itertools.combinations(particle_bag.values(), 2):
    collision_times.update(get_collision_times(particle_a, particle_b))

#print("Debug collision times", sorted(collision_times))
for collision_time in sorted(collision_times):
    if collision_time < 0:
        continue
    pos_bag = dict((k, at_time(v, collision_time)) for (k, v) in particle_bag.items())
    re_org_pos = {}
    for (k, v) in pos_bag.items():
        re_org_pos.setdefault(v, []).append(k)
    eliminated = set()
    for (k, v) in re_org_pos.items():
        if len(v) > 1:
            eliminated.update(v)
            for idx in v:
                del particle_bag[idx]
    # print("debug:", collision_time, len(particle_bag), "eliminated", sorted(eliminated))

print(len(particle_bag))
