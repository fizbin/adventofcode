import sys
from collections import defaultdict
Positions = []
CurPosition = 0 + 0j
DistanceField = defaultdict(int)
for CurChar in sys.stdin.read()[1:-1]:
	if CurChar   == '(':
		Positions.append(CurPosition)
	elif CurChar == ')':
		CurPosition = Positions.pop()
	elif CurChar == '|':
		CurPosition = Positions[-1]
	elif CurChar in 'NESW':
		NewPosition = CurPosition + {'N':-1j,'E':1,'S':1j,'W':-1}[CurChar]
		DistanceField[NewPosition] = min(
			DistanceField.get(NewPosition,DistanceField[CurPosition]+1),
			DistanceField[CurPosition]+1
		)
		CurPosition = NewPosition

print("Part 1: " + str(max(DistanceField.values())))
print("Part 2: " + str(sum(map(lambda x: x >= 1000, DistanceField.values()))))

