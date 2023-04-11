import sys

myinput = 376
if len(sys.argv) > 1:
    myinput = int(sys.argv[1])

links_fore = [0]
links_back = [0]

for last_insert in range(2018):
    cp = last_insert
    for _ in range(myinput):
        cp = links_fore[cp]
    ncp = links_fore[cp]
    links_fore.append(ncp)
    links_back.append(cp)
    links_fore[cp] = last_insert + 1
    links_back[ncp] = last_insert + 1

print(links_fore[2017])

next_to_zero = 0
cp = 0  # how far from 0
for to_insert in range(1, 50000000 + 1):
    cp += myinput
    cp %= to_insert
    if cp == 0:
        next_to_zero = to_insert
    cp += 1

print(next_to_zero)
