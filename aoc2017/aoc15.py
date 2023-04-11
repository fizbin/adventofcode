import numpy as np
import sys

start_a = 679
start_b = 771

if len(sys.argv) >= 3:
    start_a = int(sys.argv[1])
    start_b = int(sys.argv[2])

a_mul0 = 16807
b_mul0 = 48271
base = 2147483647

a_arr0 = np.array([(a_mul0 * start_a) % base], dtype=np.uint64)
b_arr0 = np.array([(b_mul0 * start_b) % base], dtype=np.uint64)


def double_arrays(a_arr, a_mul, b_arr, b_mul):
    new_a_arr = a_arr.copy()
    new_b_arr = b_arr.copy()
    new_a_arr *= a_mul
    new_a_arr %= base
    new_b_arr *= b_mul
    new_b_arr %= base
    return (
        np.concatenate((a_arr, new_a_arr)),
        (a_mul * a_mul) % base,
        np.concatenate((b_arr, new_b_arr)),
        (b_mul * b_mul) % base,
    )


work_tuple = (a_arr0, a_mul0, b_arr0, b_mul0)

forty_million = 40 * 1000 * 1000
five_million = 5 * 1000 * 1000
divn = 10
arrlim = forty_million // divn
while len(work_tuple[0]) < arrlim:
    work_tuple = double_arrays(*work_tuple)

work_a_arr = work_tuple[0][0:arrlim].copy()
work_a_mul = pow(a_mul0, arrlim, base)
work_b_arr = work_tuple[2][0:arrlim].copy()
work_b_mul = pow(b_mul0, arrlim, base)
count1 = 0
workrounds = 0
new_a_out = np.array([], dtype=np.uint64)
new_b_out = np.array([], dtype=np.uint64)
while (
    (workrounds < divn)
    or (len(new_a_out) < five_million)
    or (len(new_b_out) < five_million)
):
    if workrounds < divn:
        count1 += ((work_a_arr & 0xFFFF) == (work_b_arr & 0xFFFF)).sum()
    # collect new vals
    if len(new_a_out) < five_million:
        new_a_out = np.concatenate(
            (new_a_out, work_a_arr[work_a_arr % 4 == 0] & 0xFFFF)
        )
    if len(new_b_out) < five_million:
        new_b_out = np.concatenate(
            (new_b_out, work_b_arr[work_b_arr % 8 == 0] & 0xFFFF)
        )
    work_a_arr *= work_a_mul
    work_a_arr %= base
    work_b_arr *= work_b_mul
    work_b_arr %= base
    workrounds += 1
    # print(workrounds)

print(count1)
print(np.sum(new_a_out[0:five_million] == new_b_out[0:five_million]))
