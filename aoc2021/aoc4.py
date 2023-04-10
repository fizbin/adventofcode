import sys
import re
import itertools

num_called = []
boards = []
with open('aoc4.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = re.findall(r'\S+', f.read())
    num_called = [int(x) for x in data[0].split(",")]
    data = data[1:]
    for start_idx in range(0, len(data), 25):
        boards.append([int(x) for x in data[start_idx:start_idx+25]])

def won(board, called):
    cset = set(called)
    bc = [int(val in cset) for val in board]
    for horiz in range(5):
        if sum(bc[horiz*5:horiz*5+5]) == 5:
            return True
    for vert in range(5):
        if bc[vert] + bc[vert + 5] + bc[vert + 10] + bc[vert + 15] + bc[vert + 20] == 5:
            return True
    return False

def score(board, called):
    cset = set(called)
    uncalled = sum([val for val in board if val not in cset])
    last_called = called[-1]
    return last_called * uncalled


done = False
for game_idx in range(4, len(num_called)):
    sofar = num_called[0:game_idx]
    for board in boards:
        if won(board, sofar):
            print(score(board, sofar))
            done = True
            break
    if done:
        break


done = False
workboards = boards
for game_idx in range(4, len(num_called)):
    sofar = num_called[0:game_idx]
    for (idx, board) in list(reversed(list(enumerate(workboards)))):
        if won(board, sofar):
            if len(workboards) == 1:
                print(score(board, sofar))
                done = True
                break
            del workboards[idx]
    if done:
        break
