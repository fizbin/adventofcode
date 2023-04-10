import re
import itertools
import numpy as np

recipes = [3, 7]
elf1 = 0
elf2 = 1

while True:
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
    if recipes[-6:] == [3,2,7,9,0,1]:
        print (len(recipes)-6)
        break
    if recipes[-7:-1] == [3,2,7,9,0,1]:
        print (len(recipes)-7)
        break


