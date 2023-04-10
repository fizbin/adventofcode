import numpy as np

NXT = 0
PRV = 1


def play_marbles(player_count, last_marble):
    marbles = np.zeros(shape=(last_marble + 1, 2), dtype=int)
    turn = 1
    player = 1
    scores = [0] * player_count
    current_marble = [0]

    def fwd():
        current_marble[0] = marbles[current_marble[0], NXT]
    def rev():
        current_marble[0] = marbles[current_marble[0], PRV]
    def drop():
        new_curr = marbles[current_marble[0], NXT]
        old_curr = current_marble[0]
        marbles[new_curr, PRV] = marbles[old_curr, PRV]
        marbles[marbles[old_curr, PRV], NXT] = new_curr
        current_marble[0] = new_curr
        return old_curr
    def add(n):
        old_curr = current_marble[0]
        current_marble[0] = n
        marbles[marbles[old_curr, NXT], PRV] = n
        marbles[n, NXT] = marbles[old_curr, NXT]
        marbles[old_curr, NXT] = n
        marbles[n, PRV] = old_curr
    while turn <= last_marble:
        # if turn % 10000 == 0:
        #     print("Turn %d" % (turn,))
        if turn % 23 == 0:
            scores[player-1] += turn
            for _ in range(7):
                rev()
            scores[player-1] += drop()
        else:
            fwd()
            add(turn)

        player += 1
        turn += 1
        if player > player_count:
            player = 1
    return max(scores)

print(play_marbles(429, 7090100))
