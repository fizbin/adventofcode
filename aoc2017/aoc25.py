import sys
import re

# Begin in state A.
# Perform a diagnostic checksum after 12399302 steps.

infile = "aoc25.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = infilep.read()

tape = ([0], [])


def write(val):
    tape[0][-1] = val


def moveright():
    if tape[1]:
        tape[0].append(tape[1].pop())
    else:
        tape[0].append(0)


def moveleft():
    tape[1].append(tape[0].pop())
    if not tape[0]:
        tape[0].append(0)


# Begin in state A.
# Perform a diagnostic checksum after 12399302 steps.

m = re.match(
    r"\s*Begin in state (\w+).\s+Perform a diagnostic checksum after (\d+) steps.\s+",
    data,
)
begin_state = m.group(1)
diag_after = int(m.group(2))
remaining = data[m.end(0) :]

machine = {}

state_part_re = re.compile(
    r"If the current value is (\d+):\s+- Write the value (\d)\.\s+- Move one slot to the (\w+).\s+- Continue with state (\w+)\.\s+"
)

while remaining:
    #  In state A:
    #   If the current value is 0:
    #     - Write the value 1.
    #     - Move one slot to the right.
    #     - Continue with state B.
    #   If the current value is 1:
    #     - Write the value 0.
    #     - Move one slot to the right.
    #     - Continue with state C.
    m = re.match(r"In state (\w+):\s+", remaining)
    in_state = m.group(1)
    remaining = remaining[m.end(0) :]
    m = re.match(state_part_re, remaining)
    assert m.group(1) == "0", "Did not see for state 0 first"
    write_if_0 = int(m.group(2))
    move_if_0 = moveright if m.group(3) == "right" else moveleft
    state_if_0 = m.group(4)
    remaining = remaining[m.end(0) :]
    m = re.match(state_part_re, remaining)
    assert m.group(1) == "1", "Did not see for state 1 second"
    write_if_1 = int(m.group(2))
    move_if_1 = moveright if m.group(3) == "right" else moveleft
    state_if_1 = m.group(4)
    remaining = remaining[m.end(0) :]
    machine[in_state] = [
        (write_if_0, move_if_0, state_if_0),
        (write_if_1, move_if_1, state_if_1),
    ]

state = begin_state
for _ in range(diag_after):
    act = machine[state][tape[0][-1]]
    write(act[0])
    act[1]()
    state = act[2]

print(sum(tape[0]) + sum(tape[1]))
