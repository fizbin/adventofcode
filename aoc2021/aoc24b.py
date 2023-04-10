"""
Advent of code 2021 day 24 in python.
"""

import sys
import re


def chain_compiled(*compiled):
    def do_all(state, inplist):
        for func in compiled:
            state = func(state, inplist)
        return state

    return do_all


def compile_alu(ins):
    outspot = "wxyz".index(ins[1])
    if ins[0] == "inp":

        def do_inp(state, inplist):
            return state[0:outspot] + (inplist.pop(0),) + state[outspot + 1 :]

        return do_inp
    else:
        if re.match(r"-?\d+", ins[2]):
            inval = int(ins[2])
            if ins[0] == "add":

                def do_add_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (state[outspot] + inval,)
                        + state[outspot + 1 :]
                    )

                return do_add_const
            elif ins[0] == "mul":

                def do_mul_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (state[outspot] * inval,)
                        + state[outspot + 1 :]
                    )

                return do_mul_const
            elif ins[0] == "div":

                def do_div_const(state, inplist):
                    # Technically, assumes we only ever divide positive by positive, but safe given input
                    return (
                        state[0:outspot]
                        + (state[outspot] // inval,)
                        + state[outspot + 1 :]
                    )

                return do_div_const
            elif ins[0] == "mod":

                def do_mod_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (state[outspot] % inval,)
                        + state[outspot + 1 :]
                    )

                return do_mod_const
            elif ins[0] == "eql":

                def do_eql_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (int(state[outspot] == inval),)
                        + state[outspot + 1 :]
                    )

                return do_eql_const
        else:
            inspot = "wxyz".index(ins[2])
            if ins[0] == "add":

                def do_add_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (state[outspot] + state[inspot],)
                        + state[outspot + 1 :]
                    )

                return do_add_const
            elif ins[0] == "mul":

                def do_mul_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (state[outspot] * state[inspot],)
                        + state[outspot + 1 :]
                    )

                return do_mul_const
            elif ins[0] == "div":

                def do_div_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (state[outspot] // state[inspot],)
                        + state[outspot + 1 :]
                    )

                return do_div_const
            elif ins[0] == "mod":

                def do_mod_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (state[outspot] % state[inspot],)
                        + state[outspot + 1 :]
                    )

                return do_mod_const
            elif ins[0] == "eql":

                def do_eql_const(state, inplist):
                    return (
                        state[0:outspot]
                        + (int(state[outspot] == state[inspot]),)
                        + state[outspot + 1 :]
                    )

                return do_eql_const


def get_min_max_states(min_states, max_states, ins):
    vals = []
    for x in ins[1:]:
        if re.match(r"-?\d+", x):
            vals.append((int(x), int(x)))
        elif x in ("w", "x", "y", "z"):
            vals.append((min_states["wxyz".index(x)], max_states["wxyz".index(x)]))
        else:
            raise Exception(f"Bad ins: {ins}")
    # inp add mul div mod eql
    code = ins[0]
    if code == "inp":
        allres = (1, 9)
    elif code == "add":
        allres = [a + b for a in vals[0] for b in vals[1]]
    elif code == "mul":
        allres = [a * b for a in vals[0] for b in vals[1]]
    elif code == "div":
        allres = []
        for bot in range(vals[1][0], vals[1][1] + 1):
            if bot != 0:
                allres.append(vals[0][0] // bot)
                allres.append(vals[0][1] // bot)
    elif code == "mod":
        allres = (0,) + vals[1]
    elif code == "eql":
        allres = (0, 1)
    max_states_ret = list(max_states)
    min_states_ret = list(min_states)
    max_states_ret["wxyz".index(ins[1])] = max(allres)
    min_states_ret["wxyz".index(ins[1])] = min(allres)
    return (tuple(min_states_ret), tuple(max_states_ret))


def get_max_allowable_z(min_states, max_states, ins, max_z_out):
    """
    Find the maximum input z that might allow our output z to be <= max_z_out.

    Another way of putting it is that if input z > get_max_allowable_z(...),
    then output z is guaranteed > max_z_out
    """
    if ins[1] != "z":
        return max_z_out
    vals = []
    for x in ins[1:]:
        if re.match(r"-?\d+", x):
            vals.append((int(x), int(x)))
        elif x in ("w", "x", "y", "z"):
            vals.append((min_states["wxyz".index(x)], max_states["wxyz".index(x)]))
        else:
            raise Exception(f"Bad ins: {ins}")
    # inp add mul div mod eql
    code = ins[0]
    if code == "inp":
        raise Exception("Unexpected z input")
    elif code == "add":
        return max_z_out - vals[1][0]
    elif code == "mul":
        return max_z_out // vals[1][0]
    elif code == "div":
        max_div = vals[1][1]
        return max_div * (max_z_out + 1) - 1
    elif code == "mod":
        raise Exception("Unexpected z mod")
    elif code == "eql":
        raise Exception("Unexpected z eql")


if __name__ == "__main__":
    with open("aoc24.in" if len(sys.argv) < 2 else sys.argv[1], encoding="utf-8") as f:
        data = re.findall(
            r"[\S ]+",
            f.read(),
        )
        inlist = [tuple(re.findall(r"\S+", line)) for line in data]

    max_state = (0, 0, 0, 0)
    min_state = (0, 0, 0, 0)
    max_states = [max_state]
    min_states = [min_state]
    for (n, instr) in enumerate(inlist):
        (min_state, max_state) = get_min_max_states(min_state, max_state, instr)
        max_states.append(max_state)
        min_states.append(min_state)
    max_allowable_z = {len(inlist):0}
    prev_max_z = 0
    for (n, instr) in reversed(list(enumerate(inlist))):
        max_z = get_max_allowable_z(min_states[n], max_states[n], instr, prev_max_z)
        max_allowable_z[n] = max_z
        prev_max_z = max_z
    # statemap maps state the (min-input, max-input)
    statemap = {(0, 0, 0, 0): ("", "")}
    todo = list(enumerate(inlist))
    while todo:
        (n, instr) = todo.pop(0)
        nstatemap = {}
        if instr[0] == "inp":
            compiled = compile_alu(instr)
            for (st, v) in statemap.items():
                for d in range(1, 10):
                    res = compiled(st, [d])
                    newv0 = v[0] + str(d)
                    newv1 = v[1] + str(d)
                    if res in nstatemap:
                        (old_min, old_max) = nstatemap[res]
                        nstatemap[res] = (min(old_min, newv0), max(old_max, newv1))
                    else:
                        nstatemap[res] = (newv0, newv1)
        else:
            do_now = [instr]
            while todo and todo[0][1][0] != "inp":
                (n, instr) = todo.pop(0)
                do_now.append(instr)
            compiled = chain_compiled(*(compile_alu(i) for i in do_now))
            for (st, v) in statemap.items():
                res = compiled(st, None)
                if res[3] > max_allowable_z[n+1]:
                    continue
                if res in nstatemap:
                    nstatemap[res] = (
                        min(nstatemap[res][0], v[0]),
                        max(nstatemap[res][1], v[1]),
                    )
                else:
                    nstatemap[res] = v
        statemap = nstatemap
        print(f"Progress {n}: {len(statemap)} states")
    max_model = max(val[1] for (st, val) in statemap.items() if st[3] == 0)
    print(max_model)
    min_model = min(val[0] for (st, val) in statemap.items() if st[3] == 0)
    print(min_model)
