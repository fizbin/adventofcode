from aoc_util import get_data_lines

data = get_data_lines(7)

def recur2(lst, idx, sofar, goal):
    if idx == len(lst):
        return sofar == goal
    return recur2(lst, idx + 1, sofar + lst[idx], goal) or recur2(
        lst, idx + 1, sofar * lst[idx], goal
    )


def recur3(lst, idx, sofar, goal):
    if idx == len(lst):
        return sofar == goal
    return (
        recur3(lst, idx + 1, sofar + lst[idx], goal)
        or recur3(lst, idx + 1, sofar * lst[idx], goal)
        or recur3(lst, idx + 1, int(f"{sofar}{lst[idx]}"), goal)
    )


total = 0
extra = 0
for idx, line in enumerate(data):
    (linevalstr, numstr) = line.split(":")
    lineval = int(linevalstr)
    nums = [int(x) for x in numstr.strip().split()]
    if recur2(nums, 1, nums[0], lineval):
        total += lineval
    elif recur3(nums, 1, nums[0], lineval):
        extra += lineval

print("Part 1:", total)
print("Part 2:", total + extra)
