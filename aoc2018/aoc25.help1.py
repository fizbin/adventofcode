with open('input.txt') as f:
   stars = [tuple([int(x) for x in s.strip().split(',')]) for s in f.readlines()]

constellations = [[stars.pop(0)]]

def m_dist(a,b):
   s = 0
   for i in range(len(a)):
      s += abs(a[i] - b[i])
   return s

while len(stars) > 0:
   added = False
   for c in constellations:
      for point in c:
         i = 0
         while i < len(stars):
            curr = stars[i]
            if m_dist(point, curr) <= 3:
               c.append(stars.pop(i))
               added = True
            else:
               i+=1
   if not added:
      constellations.append([stars.pop(0)])

print(len(constellations))
