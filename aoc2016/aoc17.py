import sys
import hashlib

keystr = "njfxhljp"
if len(sys.argv) > 1:
    keystr = sys.argv[1]

key = keystr.encode()

states = [((1, 1), b"")]
printed_part1 = False
longest_to_goal = None
while states:
    ostates = states
    states = []
    for state in ostates:
        if state[0] == (4, 4):
            if not printed_part1:
                print(state[1].decode())
            printed_part1 = True
            longest_to_goal = state[1]
            continue
        hash = hashlib.md5(key + state[1]).hexdigest()
        for (movx, movy, movnom), hashchr in zip(
            [(-1, 0, b"U"), (1, 0, b"D"), (0, -1, b"L"), (0, 1, b"R")], hash
        ):
            if hashchr not in "bcdef":
                continue
            if (1 <= state[0][0] + movx <= 4) and (1 <= state[0][1] + movy <= 4):
                states.append(
                    ((state[0][0] + movx, state[0][1] + movy), state[1] + movnom)
                )

print(len(longest_to_goal))
