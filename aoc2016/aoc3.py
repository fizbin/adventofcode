import sys

infile = "aoc3.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

retval = 0
for line in data:
    nums = list(map(int, line.split()))
    if sum(nums) > 2 * max(nums):
        retval += 1
print(retval)

retval = 0
for idx, line1 in enumerate(data):
    if idx % 3:
        continue
    line2 = data[idx + 1]
    line3 = data[idx + 2]
    ns1 = list(map(int, line1.split()))
    ns2 = list(map(int, line2.split()))
    ns3 = list(map(int, line3.split()))
    for nums in [
        (ns1[0], ns2[0], ns3[0]),
        (ns1[1], ns2[1], ns3[1]),
        (ns1[2], ns2[2], ns3[2]),
    ]:
        if sum(nums) > 2 * max(nums):
            retval += 1
print(retval)
