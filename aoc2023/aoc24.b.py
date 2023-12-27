#!/usr/bin/python

import aoc_util
import itertools
import numpy as np
import math

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
    vmat = np.matrix([[-vxA, vxB], [-vyA, vyB]], dtype=float)
    try:
        tvec = np.linalg.solve(vmat, np.matrix([xA - xB, yA - yB]).transpose())
    except np.linalg.LinAlgError:
        return None
    interV = np.matrix([[xA, vxA], [yA, vyA]]) @ [1, tvec[0, 0]]
    return (interV[0, 0], interV[0, 1], tvec[0, 0], tvec[1, 0])


total = 0
for stoneA, stoneB in itertools.combinations(stones, 2):
    inter = find_intersect_2D(stoneA, stoneB)

    if inter is None:
        # It turns out I could have skipped all this stuff; this is here to
        # account for hailstones that head directly toward each other.
        #
        # In the actual input, any two hailstoanes that were parallel didn't
        # intersect, so all this was wasted code. Oh well.
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
# as vectors:
# r = s1 + t1*(s1v - rv)
# r = s2 + t2*(s2v - rv)
# r = s3 + t3*(s3v - rv)
#
# So then we have that the ray s1 + t*(s1v - rv) and the ray s2 + t*(s2v - rv)
# must intersect at "r". This means that the three vectors (s1v - rv),
# (s2v - rv) and (s1 - s2) must all lie in the same plane through the origin,
# and therefore anything perpendicular to (s1v - rv) and (s2v - rv) must be
# perpendicular to (s1-s2) as well.
#
# That is: (⨯ is the vector cross product, . is the vector dot product)
#   ((s1v - rv) ⨯ (s2v - rv)) . (s1 - s2) == 0
# and (similar equations with any two stones)
#
# This is the "triple scalar product" of our threee coplanar vectors.
# Remember for any three vectors u, v, and w:
#     (u ⨯ v) . w == (v ⨯ w) . u
#                 == (w ⨯ u) . v
#                 == -(v ⨯ u) . w
#                 == -(w ⨯ v) . u
#                 == -(u ⨯ w) . v
#
# Then:
# ((s1v - rv) ⨯ (s2v - rv)) . (s1 - s2) == 0
# (s1v ⨯ s2v - rv ⨯ s2v - s1v ⨯ rv) . (s1 - s2) == 0
# s1v ⨯ s2v . (s1-s2) - rv ⨯ s2v . (s1-s2) - s1v ⨯ rv . (s1-s2) == 0
# s1v ⨯ s2v . (s1-s2) == rv ⨯ s2v . (s1-s2) + s1v ⨯ rv . (s1-s2)
# s1v ⨯ s2v . (s1-s2) == - ((s1-s2) ⨯ s2v . rv) + ((s1-s2) ⨯ s1v . rv)
# s1v ⨯ s2v . (s1-s2) == ((s1-s2) ⨯ (s1v-s2v)) . rv
#
# then we use the fact that matrix multiplication of a matrix by a column
# vector is the same as considering each row of the matrix as a row vector
# and forming the dot product of each row with the column vector, and so
# take three equations like above and we get all the rows of a matrix
# and can plug it into np.linalg.solve to get the value of "rv"

a_rows = []
x_vals = []

stone1, stone2, stone3 = stones[0:3]
for stoneA, stoneB in (
    (stone1, stone2),
    (stone2, stone3),
    (stone3, stone1),
):
    (sAx, sAy, sAz, sAvx, sAvy, sAvz) = stoneA
    (sBx, sBy, sBz, sBvx, sBvy, sBvz) = stoneB
    sA = np.array([sAx, sAy, sAz])
    sB = np.array([sBx, sBy, sBz])
    sAv = np.array([sAvx, sAvy, sAvz])
    sBv = np.array([sBvx, sBvy, sBvz])
    x_vals.append(np.dot(np.cross(sAv, sBv), sA - sB))
    a_rows.append(np.cross(sA - sB, sAv - sBv))

rv = np.linalg.solve(a_rows, x_vals)

# Now with rv in hand, we find the intersection of the ray s1 + t*(s1v - rv)
# and the ray s2 + t*(s2v - rv) to find r:

xInter, yInter, t1, _ = find_intersect_2D(
    stone1[0:3] + tuple(np.array(stone1[3:6]) - rv), stone2[0:3] + tuple(np.array(stone2[3:6]) - rv)
)
zInter = stone1[2] + t1 * (stone1[5] - rv[2])
print(math.floor(xInter + yInter + zInter + 0.1))
