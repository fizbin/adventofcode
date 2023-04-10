class CircularDeque(object):
    def __init__(self, maxsz):
        self.impl = [None] * (maxsz + 1)
        self.impllen = maxsz + 1
        self.left = 0
        self.right = 0

    def __len__(self):
        return (self.right - self.left) % self.impllen

    def rotate_right(self):
        if self.right != self.left:
            newleft = (self.left - 1) % self.impllen
            newright = (self.right - 1) % self.impllen
            self.impl[newleft] = self.impl[newright]
            self.impl[newright] = None
            self.left = newleft
            self.right = newright

    def rotate_left(self):
        if self.right != self.left:
            newleft = (self.left + 1) % self.impllen
            newright = (self.right + 1) % self.impllen
            self.impl[self.right] = self.impl[self.left]
            self.impl[self.left] = None
            self.left = newleft
            self.right = newright

    def append(self, val):
        if len(self) >= self.impllen - 1:
            raise Exception("Would exceed max size")
        self.impl[self.right] = val
        self.right += 1
        self.right %= self.impllen

    def pop(self):
        if len(self) == 0:
            raise Exception("Empty")
        newright = (self.right - 1) % self.impllen
        retval = self.impl[newright]
        self.impl[newright] = None
        self.right = newright
        return retval


def play_marbles(player_count, last_marble):
    marbles = CircularDeque(last_marble)
    marbles.append(0)
    turn = 1
    player = 1
    scores = [0] * player_count

    while turn <= last_marble:
        # if turn % 10000 == 0:
        #     print("Turn %d" % (turn,))
        if turn % 23 == 0:
            scores[player-1] += turn
            # marbles.rotate(7)
            for _ in range(7):
                marbles.rotate_right()

            scores[player-1] += marbles.pop()
            # marbles.rotate(-1)
            marbles.rotate_left()
        else:
            # marbles.rotate(-1)
            marbles.rotate_left()
            marbles.append(turn)

        player += 1
        turn += 1
        if player > player_count:
            player = 1
    return max(scores)


print(play_marbles(429, 7090100))
