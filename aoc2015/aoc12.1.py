import sys
import re
import json


with open('aoc12.in.txt' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read()


print(sum(int(x) for x in re.findall(r'-?\d+', data)))

obj = json.loads(data, object_hook=lambda d: [] if "red" in d.values() else d)
print(sum(int(x) for x in re.findall(r'-?\d+', json.dumps(obj))))
