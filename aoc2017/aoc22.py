import sys
import copy

infile = "aoc22.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = list(infilep)

infected = set()
for lineidx, line in enumerate(data):
    line = line.strip()
    for charidx, char in enumerate(line):
        if char == "#":
            infected.add(lineidx + charidx * 1j)

ipos = (len(data) // 2) + (len(data[0].strip()) // 2) * 1j
idir = -1 + 0j

og_state = copy.deepcopy((infected, ipos, idir))


def do_move_part1():
    global ipos
    global idir
    retval = False
    if ipos in infected:
        idir *= -1j
        infected.remove(ipos)
    else:
        idir *= 1j
        infected.add(ipos)
        retval = True
    ipos += idir
    return retval


print(sum(int(do_move_part1()) for _ in range(10000)))

# part 2

(infected, ipos, idir) = og_state
node_state = {}
for node in infected:
    node_state[node] = 2


def do_move_part2():
    global ipos
    global idir
    state = node_state.get(ipos, 0)
    turn = [1j, 1 + 0j, -1j, -1 + 0j][state]
    nstate = (state + 1) % 4
    idir *= turn
    if nstate:
        node_state[ipos] = nstate
    else:
        del node_state[ipos]
    ipos += idir
    return nstate == 2


print(sum(int(do_move_part2()) for _ in range(10000000)))
