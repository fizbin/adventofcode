from aoc_util import numbers, get_data

data = numbers(get_data(20))

# data = numbers("""
# 1
# 2
# -3
# 3
# -2
# 0
# 4
# """)

ldata = len(data)
links = [[(idx - 1) % ldata, (idx+1) % ldata] for idx in range(len(data))]

for (idx, val) in enumerate(data):
    rval = val % (len(data) - 1)
    secondidx = idx
    for _ in range(rval):
        secondidx = links[secondidx][1]
    # print("R?", rval, idx, secondidx)
    if secondidx != idx:
        (pvp, pvn) = links[idx]
        (nxp, nxn) = links[secondidx]
        links[secondidx][1] = idx
        links[idx][0] = secondidx
        links[pvp][1] = pvn
        links[pvn][0] = pvp
        links[nxn][0] = idx
        links[idx][1] = nxn

    # kidx = 0
    # while True:
    #     print(data[kidx], end=' ')
    #     kidx = links[kidx][1]
    #     if kidx == 0:
    #         break
    # print()
    # print(list(zip(data, links)))

#print(order)

#print(orderinv)

zeroidx = data.index(0)
idx = zeroidx
tot = 0
for off in (1, 2, 3):
    for _ in range(1000):
        idx = links[idx][1]
    # print(data[idx])
    tot += data[idx]
print(tot)


ldata = len(data)
links = [[(idx - 1) % ldata, (idx+1) % ldata] for idx in range(len(data))]

for _ in range(10):
    for (idx, val) in enumerate(data):
        rval = (val*811589153) % (len(data) - 1)
        secondidx = idx
        for _ in range(rval):
            secondidx = links[secondidx][1]
        # print("R?", rval, idx, secondidx)
        if secondidx != idx:
            (pvp, pvn) = links[idx]
            (nxp, nxn) = links[secondidx]
            links[secondidx][1] = idx
            links[idx][0] = secondidx
            links[pvp][1] = pvn
            links[pvn][0] = pvp
            links[nxn][0] = idx
            links[idx][1] = nxn

zeroidx = data.index(0)
idx = zeroidx
tot = 0
for off in (1, 2, 3):
    for _ in range(1000):
        idx = links[idx][1]
    # print(data[idx])
    tot += data[idx]
print(tot*811589153)
