import re

with open('ac5.in.txt') as f:
    data = f.read()

data = re.sub(r'\s', '', data)

myre = '|'.join(x.upper() + x + '|' + x + x.upper() for x in 'abcdefghijklmnopqrstuvwxyz')
myre = re.compile(myre)
datap = re.sub(myre, '', data)
while datap != data:
    data = datap
    datap = re.sub(myre, '', data)

print(len(data))
