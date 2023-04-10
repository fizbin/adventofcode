import re
import itertools
import sys
import numpy as np

with open("aoc15.in.txt" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(x.strip() for x in f)

ingredients = {}
for ln in data:
    m = re.match(r'(\w+): capacity (-?\d+), durability (-?\d+),'
                 r' flavor (-?\d+), texture (-?\d+), calories (-?\d+)', ln)
    (ingredient, cap_s, dur_s, fla_s, tex_s, cal_s) = m.groups()
    ingredients[ingredient] = np.array(
        [int(cap_s), int(dur_s), int(fla_s), int(tex_s), int(cal_s)])

best_cookie_score = 0

ig_a = np.array([ingredients[i] for i in sorted(ingredients)])
icount = np.array([0] * len(ingredients)).reshape((len(ingredients), 1))
icount[-1, 0] = 100


while True:
    igsum = (icount * ig_a).sum(axis=0)
    if igsum[-1] == 500:
        score = ((igsum + abs(igsum)) // 2)[:4].prod()
        best_cookie_score = max(score, best_cookie_score)

    pull_from = np.flatnonzero(icount).max()
    if pull_from == 0:
        break
    rst = icount[pull_from, 0]
    icount[pull_from - 1, 0] += 1
    icount[pull_from:-1, 0] = 0
    icount[-1] = rst - 1

print(best_cookie_score)
