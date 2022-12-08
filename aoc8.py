from aoc_util import numpy_shift, get_data_lines
import numpy as np

data = get_data_lines(8)
trees = [list(map(int, list(r))) for r in data]

visible_trees = set()
for row in range(len(data)):
    sofar = -1
    for col in range(len(data[0])):
        if trees[row][col] > sofar:
            visible_trees.add((row,col))
            sofar = trees[row][col]
    sofar = -1
    for col in reversed(list(range(len(data[0])))):
        if trees[row][col] > sofar:
            visible_trees.add((row,col))
            sofar = trees[row][col]

for col in range(len(data[0])):
    sofar = -1
    for row in range(len(data)):
        if trees[row][col] > sofar:
            visible_trees.add((row,col))
            sofar = trees[row][col]
    sofar = -1
    for row in reversed(list(range(len(data)))):
        if trees[row][col] > sofar:
            visible_trees.add((row,col))
            sofar = trees[row][col]
    
print(len(visible_trees))

trees = np.array(trees).astype(int)

scores = np.zeros(np.shape(trees),dtype=int) * np.array([[[1]]] * 4)

for (scidx, d, axn) in [(0, 1, 0), (1, -1, 0), (2, 1, 1), (3, -1, 1)]:
    shifted = np.copy(trees)
    mask = np.ones(shape=np.shape(trees), dtype=int)
    while mask.any():
        shifted = numpy_shift(shifted, d, axn, 99)
        scores[scidx] += mask*((shifted < 90).astype(int))
        mask *= (trees > shifted).astype(int)

# There's probably a fancy numpy way to say "broadcast product across the first index",
# but I don't know what it is
final = scores[0] * scores[1] * scores[2] * scores[3]

print(final.max())
