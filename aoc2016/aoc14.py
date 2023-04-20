import hashlib
import re
import sys
import collections


hinput = "cuanljph"
if len(sys.argv) > 1:
    hinput = sys.argv[1]


class MyHashLookup(collections.defaultdict):
    def __init__(self, factory):
        self.arg_factory = factory

    def __missing__(self, __key):
        key = __key
        ans = self.arg_factory(key)
        self[key] = ans
        return ans


has3 = re.compile(r"(.)\1\1")


def is_valid(idx, lookup):
    hash = lookup[idx]
    # breakpoint()
    m = has3.search(hash)
    if m:
        target = m.group(1) * 5
        for off in range(1, 1001):
            if target in lookup[idx + off]:
                return True
    return False


hash_lookup = MyHashLookup(
    lambda key: hashlib.md5((hinput + str(key)).encode()).hexdigest()
)
idx = 0
n = int(is_valid(idx, hash_lookup))
while n < 64:
    idx += 1
    n += int(is_valid(idx, hash_lookup))

print(idx)


def stretched_lookup(idx):
    hash = hashlib.md5((hinput + str(idx)).encode()).hexdigest()
    for _ in range(2016):
        hash = hashlib.md5(hash.encode()).hexdigest()
    return hash


hash_lookup = MyHashLookup(stretched_lookup)
idx = 0
n = int(is_valid(idx, hash_lookup))
while n < 64:
    idx += 1
    n += int(is_valid(idx, hash_lookup))
    # if is_valid(idx, hash_lookup):
    #     print(idx,n)
    # elif idx % 100 == 0:
    #     print("...", idx, "...")

print(idx)
