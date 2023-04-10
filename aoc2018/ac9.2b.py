import numpy as np
from collections import deque

NXT = 0
PRV = 1


def play_marbles(player_count, last_marble):
    marbles = deque([0])
    turn = 1
    player = 1
    scores = [0] * player_count

    while turn <= last_marble:
        # if turn % 10000 == 0:
        #     print("Turn %d" % (turn,))
        if turn % 23 == 0:
            scores[player-1] += turn
            marbles.rotate(7)
            scores[player-1] += marbles.pop()
            marbles.rotate(-1)
        else:
            marbles.rotate(-1)
            marbles.append(turn)

        player += 1
        turn += 1
        if player > player_count:
            player = 1
    return max(scores)

print(play_marbles(429, 7090100))
