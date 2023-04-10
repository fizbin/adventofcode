import re
import itertools
import numpy as np

recipes = [3, 7]
elf1 = 0
elf2 = 1

while len(recipes) < 327912:
    recipe_sum = recipes[elf1] + recipes[elf2]
    if recipe_sum < 10:
        recipes.append(recipe_sum)
    else:
        recipes.append(recipe_sum // 10)
        recipes.append(recipe_sum % 10)
    elf1 += recipes[elf1] + 1
    elf2 += recipes[elf2] + 1
    elf1 = elf1 % len(recipes)
    elf2 = elf2 % len(recipes)

print(recipes[327901:327911])

