def play_marbles(player_count, last_marble):
    marbles = [0]
    turn = 1
    player = 1
    scores = [0] * player_count
    current_marble = 0
    while turn <= last_marble:
        if turn % 10000 == 0:
            print("Turn %d, marble count %d" % (turn, len(marbles)))
        if turn % 23 == 0:
            scores[player-1] += turn
            current_marble = (current_marble + (len(marbles) - 7)) % len(marbles)
            scores[player-1] += marbles.pop(current_marble)
        else:
            pre_current_next = (current_marble + 1) % len(marbles)
            marbles[pre_current_next+1:pre_current_next+1] = [turn]
            current_marble = pre_current_next + 1

        player += 1
        turn += 1
        if player > player_count:
            player = 1
    return max((s, p+1) for (p, s) in enumerate(scores))

