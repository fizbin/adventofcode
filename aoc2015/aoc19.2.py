import re
import sys
from typing import Tuple, List, Dict
import heapq


with open("aoc19.in" if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = f.read()

(rules_src, goal_molecule) = data.split("\n\n")
goal_molecule = goal_molecule.strip()
rules: List[Tuple[str, str]] = re.findall(r"(\w+) => (\w+)", rules_src)

# This solution is based on the insight that in the replacement rules, the "to"
# side of a replacement rule has one of these three shapes:
#
# - something that does not contain any of Rn, Y, or Ar
# - something that is BLAH Rn BLAH Ar where both BLAH spots are a single atoms
# - something that is BLAH Rn BLAH Y BLAH Ar where all three BLAH spots are
#   single atoms.
#
# As has been observed on reddit, you can consider Rn, Y, and Ar to function
# like the characters "(", ",", ")" in a traditional grammar.
#
# The basic strategy then is this:
# - We take the "goal" molecule and try to apply steps backwards to get to a
#   single electron.
# - if there's something that looks like Rn BLARG Ar or Rn BLARG Y or
#   Y BLARG Ar where BLARG is more than one atom long and doesn't contain any of
#   Rn, Y, or Ar then we can recursively attack BLARG to get that to a single atom.
# - if there is a replacement rule ending in Ar that applies to a molecule we
#   always apply that rule immediately and continue.
# - if there isn't such a rule, but there's a sequence like Rn X Ar or Rn X Y X Ar
#   where the X spots are single atoms, and that sequence is preceeded by Rn BLARG
#   or Y BLARG where BLARG is one or more atoms not containing Rn or Y or Ar, then
#   we only consider replacement rules that would replace something inside the
#   BLARG bit.
# - Otherwise, try any replacement rule that applies.
#
# The reasoning for that "only consider replacement rules..." piece makes the most
# sense if you think about Rn Y and Ar as parentheses and commas.

ar_rules = [x for x in rules if x[1].endswith("Ar")]
norm_rules = [x for x in rules if not x[1].endswith("Ar")]
re_cache = {}
for von, zu in rules:
    re_cache[zu] = re.compile(zu)


def reduce_molecule(smolecule: str) -> Tuple[int, str]:
    """
    Reduce the given molecule to one of a single atom/electron and return how many
    steps it took and what it was reduced to.
    """
    nsteps = 0

    # This matches Rn BLAH Y or Rn BLAH Ar or Y BLAH Ar where the "BLAH" consists
    # of two or more atoms, none of them Rn, Y, or Ar. This pattern means we can
    # recurse on the innards.
    find_recursive = re.compile(r"(?:Rn|Y)((?:(?!Ar|Y|Rn)[A-Z][a-z]?){2,})(?=Y|Ar)")

    # This matches Rn BLAH THING or Y BLAH THING, where BLAH is one or more atoms not
    # including Rn, Y, or Ar and THING is a sequence of Rn FOO Ar or Rn FOO Y FOO Ar,
    # where FOO is (possibly different) single atoms. We use this pattern after we have
    # tried all substitutions that might get rid of THING and failed; this means that
    # THING is still around because what's in front of it isn't correct yet. Therefore,
    # in that case we only allow substitutions that might affect what's in front of
    # THING.
    find_narrowing = re.compile(
        r"(?:^|Rn|Y)((?:(?!Ar|Y|Rn)[A-Z][a-z]?)+)"
        r"(?=Rn[A-Z][a-z]?Ar|Rn[A-Z][a-z]?Y[A-Z][a-z]?Ar)"
    )

    visited: Dict[str, int] = {}
    workheap = [(0, nsteps, smolecule)]
    i = 0
    while workheap:
        (_, steps, molecule) = heapq.heappop(workheap)
        if (len(molecule) == 1) or (
            len(molecule) == 2 and molecule != molecule.upper()
        ):
            # we have one atom
            return (steps, molecule)

        if m := find_recursive.search(molecule):
            (add_one, rep_one) = reduce_molecule(m.group(1))
            molecule = molecule[: m.start(1)] + rep_one + molecule[m.end(1) :]
            steps += add_one
            heapq.heappush(workheap, (steps + len(molecule), steps, molecule))
            continue

        for from_str, to_str in ar_rules:
            if m := re_cache[to_str].search(molecule):
                reduction = molecule[: m.start()] + from_str + molecule[m.end() :]
                if visited.get(reduction, steps + 10) > steps + 1:
                    visited[reduction] = steps + 1
                    tgt = (
                        steps + 1 + len(reduction),
                        steps + 1,
                        reduction,
                    )
                    heapq.heappush(workheap, tgt)
                    break
        else:
            # couldn't find any applicable ar_rules.
            # First see if that's because there's a Rn...Ar sequence that we should be
            # able to get rid of (just single atom arguments), but can't because of
            # what's in front of it.
            narr = find_narrowing.search(molecule)
            for from_str, to_str in norm_rules:
                for m in re.finditer(re_cache[to_str], molecule):
                    if narr and not (
                        narr.start(1) <= m.start() <= m.end() <= narr.end(1)
                    ):
                        # if we have a Rn ... Ar thing we can't remove because what's
                        # in front of it is wrong, ignore every replacement that isn't
                        # fixing that bit.
                        continue
                    reduction = molecule[: m.start()] + from_str + molecule[m.end() :]
                    if visited.get(reduction, steps + 10) > steps + 1:
                        visited[reduction] = steps + 1
                        tgt = (
                            steps + 1 + len(reduction),
                            steps + 1,
                            reduction,
                        )
                        heapq.heappush(workheap, tgt)

    assert False, smolecule


print(reduce_molecule(goal_molecule))
