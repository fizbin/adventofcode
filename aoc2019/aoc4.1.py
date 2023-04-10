#!/usr/bin/python
import sys
import re

input = (372037, 905157)

count = 0

for d1 in range(3, 10):
    for d2 in range(d1, 10):
        for d3 in range(d2, 10):
            for d4 in range(d3, 10):
                for d5 in range(d4, 10):
                    for d6 in range(d5, 10):
                        if len(set([d1,d2,d3,d4,d5,d6])) < 6:
                            nval = ((((d1*10 + d2)*10 + d3)*10+d4)*10+d5)*10+d6
                            if nval >= input[0] and nval <= input[1]:
                                count += 1

print(count)
