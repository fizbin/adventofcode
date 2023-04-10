from collections import *
r=open('d').read()
d=defaultdict(lambda:1e9)
p=d[0]=0
s=[]
for c in r[1:-1]:
 if'('==c:s.append(p)
 elif')'==c:p=s.pop()
 elif'|'==c:p=s[-1]
 else:l=p;p+={'S':1j,'N':-1j,'E':1,'W':-1}[c];d[p]=min(d[p],d[l]+1)
v=d.values()
print(max(v),sum(x>=1e3 for x in v))
