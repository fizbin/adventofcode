import re


def shrinkdata(data):
    myre = '|'.join(x.upper() + x + '|' + x + x.upper()
                    for x in 'abcdefghijklmnopqrstuvwxyz')
    myre = re.compile(myre)
    datap = re.sub(myre, '', data)
    while datap != data:
        data = datap
        datap = re.sub(myre, '', data)
    return data


with open('ac5.in.txt') as f:
    data = f.read()

data = re.sub(r'\s', '', data)

# This line helps a lot
data = shrinkdata(data)

minlength = None
for x in 'abcdefghijklmnopqrstuvwxyz':
    datax = re.sub(x, '', data, flags=re.I)
    l = shrinkdata(datax)
    if minlength is None:
        minlength = len(l)
    else:
        minlength = min(len(l), minlength)
    print((x, len(l)))

print(minlength)
