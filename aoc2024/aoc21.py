from aoc_util import get_data_lines
import functools

data = get_data_lines(21)

num_map_str = "789\n456\n123\n 0A\n"
num_pad_rev = {}
num_pad_map = {}
for rowidx, row in enumerate(num_map_str.splitlines()):
    for colidx, ch in enumerate(row):
        if ch != " ":
            num_pad_map[(rowidx, colidx)] = ch
            num_pad_rev[ch] = (rowidx, colidx)

dir_map_str = " ^A\n<v>\n"
dir_pad_rev = {}
dir_pad_map = {}
for rowidx, row in enumerate(dir_map_str.splitlines()):
    for colidx, ch in enumerate(row):
        if ch != " ":
            dir_pad_map[(rowidx, colidx)] = ch
            dir_pad_rev[ch] = (rowidx, colidx)


def get_numpad_seq_len(code, nintermed=2):
    pspot = "A"
    dist = 0
    for spot in code:
        dist += get_shortest_numpad1_len(spot, pspot, nintermed)
        pspot = spot
    return dist


def get_shortest_numpad1_len(code: str, pcode: str, nintermed=2) -> int:
    prev_pos = num_pad_rev[pcode]
    new_pos = num_pad_rev[code]
    xoff, yoff = (new_pos[0] - prev_pos[0], new_pos[1] - prev_pos[1])
    axoff = abs(xoff)
    ayoff = abs(yoff)
    if xoff < 0:
        xdir = "^"
    else:
        xdir = "v"
    if yoff < 0:
        ydir = "<"
    else:
        ydir = ">"
    allowed = []
    if prev_pos[1] == 0 and new_pos[0] == 3:
        allowed = [ydir * ayoff + xdir * axoff]
    elif new_pos[1] == 0 and prev_pos[0] == 3:
        allowed = [xdir * axoff + ydir * ayoff]
    else:
        allowed = sorted(set([ydir * ayoff + xdir * axoff, xdir * axoff + ydir * ayoff]))

    min_allowed = 9999999999999999999999
    for x in allowed:
        min_allowed = min(min_allowed, get_dirpad_seq_len(x + "A", nintermed))
    return min_allowed


def get_dirpad_seq_len(code, level):
    if level == 0:
        return len(code)
    pspot = "A"
    dist = 0
    for spot in code:
        dist += get_shortest_dirpad1_len(spot, pspot, level)
        pspot = spot
    return dist


@functools.cache
def get_shortest_dirpad1_len(code: str, pcode: str, level: int) -> int:
    prev_pos = dir_pad_rev[pcode]
    new_pos = dir_pad_rev[code]
    xoff, yoff = (new_pos[0] - prev_pos[0], new_pos[1] - prev_pos[1])
    axoff = abs(xoff)
    ayoff = abs(yoff)
    if xoff < 0:
        xdir = "^"
    else:
        xdir = "v"
    if yoff < 0:
        ydir = "<"
    else:
        ydir = ">"
    allowed = []
    if prev_pos[1] == 0 and new_pos[0] == 0:
        allowed = [ydir * ayoff + xdir * axoff]
    elif new_pos[1] == 0 and prev_pos[0] == 0:
        allowed = [xdir * axoff + ydir * ayoff]
    else:
        allowed = sorted(set([ydir * ayoff + xdir * axoff, xdir * axoff + ydir * ayoff]))

    min_allowed = 9999999999999999999999
    for x in allowed:
        min_allowed = min(min_allowed, get_dirpad_seq_len(x + "A", level - 1))
    return min_allowed


total = 0
for code in data:
    codenum = int(code[:-1], 10)
    total += codenum * get_numpad_seq_len(code)

print("Part 1:", total)


total = 0
for code in data:
    codenum = int(code[:-1], 10)
    total += codenum * get_numpad_seq_len(code, 25)

print("Part 2:", total)
