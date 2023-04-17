import sys
import re


infile = "aoc10.in"
if len(sys.argv) > 1:
    infile = sys.argv[1]

with open(infile, "r", encoding="utf-8") as infilep:
    data = [x.strip() for x in list(infilep)]

# value 3 goes to bot 1
# bot 1 gives low to output 1 and high to bot 0

bot_vals: dict[int, list[int]] = {}
bot_does = {}
for line in data:
    m = re.match(r"value (\d+) goes to bot (\d+)", line)
    if m:
        bot_vals.setdefault(int(m.group(2)), []).append(int(m.group(1)))
        continue
    m = re.match(
        r"bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)",
        line,
    )
    if m:
        bot_does[int(m.group(1))] = (
            m.group(2),
            int(m.group(3)),
            m.group(4),
            int(m.group(5)),
        )
        continue
    raise Exception(f"Uninterpreted {line!r}")
output_vals: dict[int, list[int]] = {}

done = False
while not done:
    done = True
    for bot, val in sorted(bot_vals.items()):
        if len(val) < 2:
            continue
        done = False
        if sorted(val) == [17, 61]:
            print(bot)
        del bot_vals[bot]
        does = bot_does[bot]
        if does[0] == "output":
            output_vals.setdefault(does[1], []).append(min(val))
        else:
            bot_vals.setdefault(does[1], []).append(min(val))
        if does[2] == "output":
            output_vals.setdefault(does[3], []).append(max(val))
        else:
            bot_vals.setdefault(does[3], []).append(max(val))

print(output_vals[0][0] * output_vals[1][0] * output_vals[2][0])
