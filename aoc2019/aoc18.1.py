import sys
import collections
import string
import heapq
import queue

with open('aoc18.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

start = None
keys = {}
maze = {}
for (y, line) in enumerate(data):
    for (x, ch) in enumerate(line):
        maze[y + (0+1j)*x] = ch
        if ch in string.ascii_lowercase:
            keys[ch] = y + (0+1j)*x
        if ch == '@':
            start = y + (0+1j)*x


def visible_profile_for(spot1):
    best = {}
    workq = queue.Queue()
    workq.put((spot1, 0, frozenset()))
    while not workq.empty():
        (zspot, dist, needed_keys) = workq.get()
        bestlist = best.setdefault(zspot, [])
        for (pdist, pkeys) in bestlist:
            if (((pdist <= dist) and (pkeys < needed_keys)) or
                ((pdist < dist) and (pkeys <= needed_keys))):
                break
        else:
            bestlist.append((dist, needed_keys))
            if maze[zspot] in string.ascii_letters:
                needed_keys = needed_keys | frozenset(maze[zspot].lower())
            for nextz in (zspot + 1, zspot - 1, zspot + 1j, zspot - 1j):
                if maze[nextz] == '#':
                    continue
                workq.put((nextz, dist + 1, needed_keys))
    return [(keych, dist, needed_keys)
            for keych in keys
            for (dist, needed_keys) in best[keys[keych]]]

visible_profiles_for = {}

def visible_stop_points(currentz, found_keys):
    if currentz not in visible_profiles_for:
        visible_profiles_for[currentz] = visible_profile_for(currentz)
    for (keych, dist, needed_keys) in visible_profiles_for[currentz]:
        if (needed_keys <= found_keys) and (keych not in found_keys):
            yield (keych, dist)
    return

def prior_adj(currentz, found_keys):
    if currentz not in visible_profiles_for:
        visible_profiles_for[currentz] = visible_profile_for(currentz)
    return max(0, *(dist
                    for (keych, dist, needed_keys)
                    in visible_profiles_for[currentz]
                    if keych not in found_keys))

named_spots = dict(keys.items())
named_spots['@'] = start
workheap = [(0, 0, frozenset(), '@')]

visited = set()
added = set()
maxklen = 0

while workheap:
    (ignored_priority, dist, keys_in_hand, spotname) = heapq.heappop(workheap)
    if (keys_in_hand, spotname) in visited:
        continue
    visited.add((keys_in_hand, spotname))
    if len(keys_in_hand) == len(keys):
        print("Minimum distance", dist)
        break
    if len(keys_in_hand) > maxklen:
        maxklen = len(keys_in_hand)
        print(maxklen)
    spot = named_spots[spotname]
    for (next_key, dist_to_next) in visible_stop_points(spot,
                                                        keys_in_hand):
        new_dist = dist + dist_to_next
        new_keys = keys_in_hand | frozenset([next_key])
        new_prior = new_dist  # + prior_adj(named_spots[next_key], new_keys)
        to_add = (new_prior, new_dist, new_keys, next_key)
        if to_add in added:
            continue
        heapq.heappush(workheap, to_add)
        added.add(to_add)


print("done")
