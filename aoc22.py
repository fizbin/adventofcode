"""
Advent of code 2021 day 22 in python.
"""

import sys
import re
import numpy as np

if __name__ == "__main__":
    with open("aoc22.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        data = re.findall(r'(on|off) x=([-\d]+)\.\.([-\d]+),y=([-\d]+)\.\.([-\d]+),z=([-\d]+)\.\.([-\d]+)', f.read())
        fmtdata = [(t[0] == 'on', (int(t[1]),int(t[2])),
                    (int(t[3]),int(t[4])),
                    (int(t[5]),int(t[6]))
                   ) for t in data]
    print(len(fmtdata))
    print((fmtdata[0]))
    stuff = np.zeros((1000,1000,1000), dtype=np.int8)
    for blk in fmtdata:
        if -60 < blk[1][0] < 60:
            stuff[blk[1][0]+50:blk[1][1]+51,blk[2][0]+50:blk[2][1]+51,blk[3][0]+50:blk[3][1]+51] = int(blk[0])
    print(stuff.sum())

    all_x = sorted(sum([[blk[1][0],blk[1][1]+1] for blk in fmtdata], []))
    all_y = sorted(sum([[blk[2][0],blk[2][1]+1] for blk in fmtdata], []))    
    all_z = sorted(sum([[blk[3][0],blk[3][1]+1] for blk in fmtdata], []))
    stuff = np.zeros((len(all_x),len(all_y),len(all_z)), dtype=np.int64)
    for blk in fmtdata:
        ix1 = all_x.index(blk[1][0])
        ix2 = all_x.index(blk[1][1]+1)
        iy1 = all_y.index(blk[2][0])
        iy2 = all_y.index(blk[2][1]+1)
        iz1 = all_z.index(blk[3][0])
        iz2 = all_z.index(blk[3][1]+1)
        stuff[ix1:ix2,iy1:iy2,iz1:iz2] = int(blk[0])
    print(stuff.sum())
    print(stuff.shape)
    def szfunc(x, ax):
        xdim = (ax + [ax[-1] + 1])[x:x+2]
        return (xdim[1] - xdim[0])
    xfoo = np.array([szfunc(x, all_x) for x in range(len(all_x))]).reshape((len(all_x), 1, 1))
    yfoo = np.array([szfunc(y, all_y) for y in range(len(all_y))]).reshape((1, len(all_y), 1))
    zfoo = np.array([szfunc(z, all_z) for z in range(len(all_z))]).reshape((1,1,len(all_z)))
    sz = xfoo * yfoo * zfoo
    print(sz.shape)
    print((stuff * sz).sum())
