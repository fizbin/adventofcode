import re

with open('ac8.in.txt') as f:
    data = f.read()

nums = list(int(x) for x in re.findall(r'\d+', data))

def read_node(input):
    child_count = input.pop(0)
    md_count = input.pop(0)
    child_vals = []
    for _ in range(child_count):
        child_vals.append(read_node(input))
    tot = 0
    for _ in range(md_count):
        if child_count == 0:
            tot += input.pop(0)
        else:
            child_num = input.pop(0)
            if child_num > 0 and child_num <= child_count:
                tot += child_vals[child_num - 1]
    return tot


print(read_node(list(nums)))
