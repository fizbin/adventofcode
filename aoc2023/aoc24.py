#!/usr/bin/python

import aoc_util
import itertools

data = aoc_util.get_data_lines(24)

stones = []
for line in data:
    stones.append(tuple(aoc_util.numbers(line)))

min_coord = 200000000000000
max_coord = 400000000000000
# min_coord = 7
# max_coord = 27


def find_intersect_2D(stoneA, stoneB):
    (xA, yA, _1, vxA, vyA, _2) = stoneA
    (xB, yB, _1, vxB, vyB, _2) = stoneB
    #
    # xA + t1*vxA = xB + t2*vxB
    # yA + t1*vyA = yB + t2*vyB
    #
    # xA - xB = (-vxA  vxB) (t1)
    # yA - yB = (-vyA  vyB) (t2)
    #
    vmat = [[-vxA, vxB], [-vyA, vyB]]
    vmatdet = vmat[0][0] * vmat[1][1] - vmat[0][1] * vmat[1][0]
    if vmatdet == 0:
        # print(f"Returning none for {stoneA} {stoneB}")
        return None
    vmati = [[vmat[1][1], -vmat[0][1]], [-vmat[1][0], vmat[0][0]]]
    t1 = vmati[0][0] * (xA - xB) + vmati[0][1] * (yA - yB)
    t1 /= vmatdet
    t2 = vmati[1][0] * (xA - xB) + vmati[1][1] * (yA - yB)
    t2 /= vmatdet
    xInter = xA + t1 * vxA
    yInter = yA + t1 * vyA
    return (xInter, yInter, t1, t2)


total = 0
for stoneA, stoneB in itertools.combinations(stones, 2):
    inter = find_intersect_2D(stoneA, stoneB)

    if inter is None:
        for horizLine in (min_coord, max_coord):
            interA = find_intersect_2D((min_coord, min_coord, 0, 1, 0, 0), stoneA)
            interB = find_intersect_2D((min_coord, min_coord, 0, 1, 0, 0), stoneB)
            (iax, iay, t1a, t2a) = interA
            (ibx, iby, t1b, t2b) = interB
            if t1a < 0 or t1b < 0 or t2a < 0 or t2b < 0:
                interA = find_intersect_2D((max_coord, min_coord, 0, 1, 0, 0), stoneA)
                interB = find_intersect_2D((max_coord, min_coord, 0, 1, 0, 0), stoneB)
                (iax, iay, t1a, t2a) = interA
                (ibx, iby, t1b, t2b) = interB
            if t1a < 0 or t1b < 0 or t2a < 0 or t2b < 0:
                interA = find_intersect_2D((min_coord, min_coord, 0, 0, 1, 0), stoneA)
                interB = find_intersect_2D((min_coord, min_coord, 0, 0, 1, 0), stoneB)
                (iax, iay, t1a, t2a) = interA
                (ibx, iby, t1b, t2b) = interB
            if t1a < 0 or t1b < 0 or t2a < 0 or t2b < 0:
                interA = find_intersect_2D((min_coord, max_coord, 0, 0, 1, 0), stoneA)
                interB = find_intersect_2D((min_coord, max_coord, 0, 0, 1, 0), stoneB)
                (iax, iay, t1a, t2a) = interA
                (ibx, iby, t1b, t2b) = interB
            if t1a < 0 or t1b < 0 or t2a < 0 or t2b < 0:
                continue
            if iax - 0.01 <= ibx <= iax + 0.01 and iay - 0.01 <= iby <= iay + 0.01:
                total += 1
                break
    # xInter = xA + t1*vxA
    # yInter = yA + t1*vyA
    else:
        xInter, yInter, t1, t2 = inter
        # print(f"{stoneA} {stoneB} --> {inter}")
        if t1 >= 0 and t2 >= 0:
            if min_coord <= xInter <= max_coord:
                if min_coord <= yInter <= max_coord:
                    total += 1

print(total)


#
# rx + t1*rvx = s1x + t1*s1vx
# ry + t1*rvy = s1y + t1*s1vy
# rz + t1*rvz = s1z + t1*s1vz
#
# rx + t1*(rvx - s1vx) = s1x
# ry + t1*(rvy - s1vy) = s1y
# rz + t1*(rvz - s1vz) = s1z
#
# So 3*n equations and 6 + n unknowns
# So if this is possible, the first three stones should be sufficient
#
# rx + t1*(rvx - s1vx) = s1x
# ry + t1*(rvy - s1vy) = s1y
# rz + t1*(rvz - s1vz) = s1z
#
# rx + t2*(rvx - s2vx) = s2x
# ry + t2*(rvy - s2vy) = s2y
# rz + t2*(rvz - s2vz) = s2z
#
# rx + t3*(rvx - s3vx) = s3x
# ry + t3*(rvy - s3vy) = s3y
# rz + t3*(rvz - s3vz) = s3z
#
# Or, the first four hailstones should be enough just working in x and y

# Ugh. IDK. Let Mathematica (well, "Wolfram Cloud") solve it:

print()
print("(* Feed the following into wolfram cloud as one cell, then evaluate *)")
print("r = Solve[{")
for idx, stone in enumerate(stones[0:3]):
    print(f"rx + t{idx}*(rvx - ({stone[3]})) == {stone[0]},")
    print(f"ry + t{idx}*(rvy - ({stone[4]})) == {stone[1]},")
    print(f"rz + t{idx}*(rvz - ({stone[5]})) == {stone[2]}{'' if idx==2 else ','}")
print("}, {rx, ry, rz, rvx, rvy, rvz, t0, t1, t2}];")
print("(rx + ry + rz) //. r[[1]]")
