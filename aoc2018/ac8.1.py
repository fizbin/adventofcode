import re

with open('ac8.in.txt') as f:
    data = f.read()

nums = list(int(x) for x in re.findall(r'\d+', data))

def read_node(input):
    child_count = input.pop(0)
    md_count = input.pop(0)
    tot = 0
    for _ in range(child_count):
        tot += read_node(input)
    for _ in range(md_count):
        tot += input.pop(0)
    return tot

print(read_node(list(nums)))
