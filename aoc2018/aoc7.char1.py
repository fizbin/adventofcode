# Day 7: The Sum of Its Parts
# Date: 12.21.2018
# Author: Kevin Tan

# create adjacency matrix for directed graph
import numpy as np
import sys
adjMtx = np.zeros((26, 26))

# populate matrix with data
# prereqData = open("puzzle-inputs/train.txt", "r")
prereqData = open("puzzle-inputs/test.txt" if len(sys.argv) < 2 else sys.argv[1], "r")

prereqDataString = ''
for prereq in prereqData:
    prereqDataString += prereq
    start = ord(prereq[5]) - 65
    end = ord(prereq[36]) - 65
    adjMtx[start, end] = 1
print("Adjacency Matrix:", adjMtx)

# find letters in alphabet that are not a step (i.e. 'L')
# only valid for training input (test uses all letters)
import string
notInAlphabet = set([])
for letter in string.ascii_uppercase:
    if " " + letter + " " not in prereqDataString:
        notInAlphabet.add(ord(letter) - 65)
print("Letters that are not steps:", notInAlphabet)

# find starting points (steps with no prerequisites)
startPoints = []
for i in range(26):
    if sum(adjMtx[:,i]) == 0:
        startPoints.append(i)
print("Starting Points:", startPoints)

def prerexSatisfied(curr : int, adjMtx, completed : set):
    """ Returns a boolean value representing if all the prerequisites for the
    curr step have been satisfied or not. """
    incomingSteps = adjMtx[:,curr]
    for i in range(len(incomingSteps)):
        if incomingSteps[i] == 1 and i not in completed:
            return False
    return True

def executeInstructions(curr : int, adjMtx, completed : set, order : list):
    """ Returns string indicating order to execute steps given prerequisite info
    from an adjacency matrix. Takes an int as the current node, a set containing
    all completed steps, and modifies a list of the order in which steps should
    be executed. """

    # backtracking case: prerequisites not satisfied or step already done
    if not prerexSatisfied(curr, adjMtx, completed) or curr in completed:
        return

    # recursive case: execute step and try to execute successive steps
    order.append(curr)
    completed.add(curr)
    nextSteps = adjMtx[curr,:]
    for i in range(len(nextSteps)):
        if nextSteps[i] == 1:
            executeInstructions(i, adjMtx, completed, order)

# loop over starting points and execute instructions
completed = set()
order = []
for startPoint in startPoints:
    executeInstructions(startPoint, adjMtx, completed, order)

# convert numerical order into a string
stringOrder = ""
for i in range(len(order)):
    if i not in notInAlphabet:
        order[i] = chr(order[i] + 65)
        stringOrder += order[i]

print("Order to execute instructions:", stringOrder)
