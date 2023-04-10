import sys
import re

data = '1321131112' if len(sys.argv) < 2 else sys.argv[1]

digs = data
for _ in range(50):
    newdigs = ''
    for k in re.findall('0+|1+|2+|3+|4+|5+|6+|7+|8+|9+', digs):
        newdigs += '%d%s' % (len(k), k[0])
    digs = newdigs

print(len(digs))
