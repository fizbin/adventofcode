import sys
import re

def flood_grid(row=0, col=500, path=[(0, 0)]):
  try:
    if row > y_max - 1:
        return 0

    if grid[row + 1][col] == ".":
        grid[row + 1][col] = "~"
        flood_grid(row + 1, col, path + [(row, col)])

    elif "." in [grid[row][col + 1], grid[row][col - 1]]:
        if grid[row][col - 1] == ".":
            grid[row][col - 1] = "~"
            flood_grid(row, col - 1, path + [(row, col)])

        if grid[row][col + 1] == ".":
            grid[row][col + 1] = "~"
            flood_grid(row, col + 1, path + [(row, col)])
    else:
        (i, j) = path.pop()
        flood_grid(i, j, path)

  except IndexError:
    print('\n'.join(''.join(grid[r][x_min:x_max+1]) for r in range(y_min,y_max+1)))
    print((row,col,len(path)))
    raise


sys.setrecursionlimit(5500)
with open("input.txt" if len(sys.argv) < 2 else sys.argv[1], "r") as infile:
    data = infile.read()

x_values = [int(x[2:]) for x in re.findall("x=\d+", data)] + [int(x.group(1)) for x in re.finditer(r"x=\d+\.\.(\d+)", data)]
y_values = [int(y[2:]) for y in re.findall("y=\d+", data)] + [int(y.group(1)) for y in re.finditer(r"y=\d+\.\.(\d+)", data)]
x_min, x_max, y_min, y_max = min(x_values), max(x_values), min(y_values), max(y_values)
grid = [["." for x in range(x_max + 25)] for y in range(y_max + 5)]
grid[0][500] = "+"
total = 0

for line in data.splitlines():
    a, b = line.split(",")
    m, n = [int(x) for x in re.findall("\d+", b)]
    j = int(a[2:])
    for i in range(m, n + 1):
        if "x" in b:
            grid[j][i] = "#"
        else:
            grid[i][j] = "#"

flood_grid()
for i in range(1, y_max + 1):
    for j in range(x_max + 1):
        if grid[i][j] == "~":
            total += 1

print('\n'.join(''.join(grid[r][x_min:x_max+1]) for r in range(y_min,y_max+1)))
print((row,col,len(path)))
print(total)

