class Stack:
    def __init__(self):
        self.items = []

    def push(self, entry):
        return self.items.insert(0, entry)

    def is_empty(self):
        return len(self.items) == 0

    def pop(self):
        return self.items.pop(0)

    def print_items(self):
        print(self.items)

    def print_len(self):
        print(len(self.items))

    def length(self):
        return len(self.items)


with open("dane 5.txt") as data:
    word = data.read()
    word = word.rstrip()

test = Stack()

for x in range(len(word)):
    print("Tura numer: ", x)
    if test.length() == 0:
        test.push(word[x])
        continue
    
    tried = test.pop()
    
    if word[x].upper() == tried.upper():
        if word[x].isupper() and tried.islower():
            continue
        elif word[x].islower() and tried.isupper():
            continue

    test.push(tried)
    test.push(word[x])

test.print_len()
