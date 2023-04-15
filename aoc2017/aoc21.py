import sys
import math
import numpy as np

infile = "aoc21.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

book = {}
for line in data:
    (before, _, after) = line.split()
    sqsz = int(math.sqrt(len(after.replace("/", ""))))
    book[before.replace("/", "").encode("utf-8")] = np.frombuffer(
        after.replace("/", "").encode(), dtype="|S1"
    ).reshape((sqsz, sqsz))


def do_xform(arr: np.ndarray):
    arrsz = arr.shape[0]
    if (arrsz % 2) == 0:
        retval = np.zeros(((arrsz // 2) * 3, (arrsz // 2) * 3), dtype="|S1")
        a = 2
        b = 3
    else:
        assert arrsz % 3 == 0
        retval = np.zeros(((arrsz // 3) * 4, (arrsz // 3) * 4), dtype="|S1")
        a = 3
        b = 4
    src_arr = np.zeros((a, a), dtype="|S1")
    for i in range(arrsz // a):
        for j in range(arrsz // a):
            src_arr[...] = arr[i * a : (i + 1) * a, j * a : (j + 1) * a]
            initial = b"".join(src_arr.ravel())
            look = b""
            for _ in range(4):
                look = b"".join(src_arr.ravel())
                if look in book:
                    break
                src_arr[...] = np.rot90(src_arr)
            else:
                src_arr[...] = src_arr.transpose()
                for _ in range(4):
                    look = b"".join(src_arr.ravel())
                    if look in book:
                        break
                    src_arr[...] = np.rot90(src_arr)
                else:
                    raise Exception(f"No lookup found for {initial!r}")
            book[initial] = book[look]
            retval[b * i : b * (i + 1), b * j : b * (j + 1)] = book[look]
    return retval


# start shape:

# .#.
# ..#
# ###

pattern = np.frombuffer(b".#...####", dtype="|S1").reshape((3, 3))
for _ in range(5):
    pattern = do_xform(pattern)
print((pattern == b"#").astype(int).sum())

for _ in range(13):
    pattern = do_xform(pattern)
print((pattern == b"#").astype(int).sum())
