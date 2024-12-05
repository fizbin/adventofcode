from aoc_util import get_data_paras

(rules, books) = get_data_paras(5)

ruleset = set(rules.splitlines())

total = 0
out_of_order = []
for book in books.splitlines():
    pages = book.split(",")
    for idx, val in enumerate(pages):
        if any(f"{val}|{pages[sidx]}" in ruleset for sidx in range(idx)):
            out_of_order.append(pages)
            break
    else:
        total += int(pages[len(pages)//2])
print("Part 1:", total)

def sort_book(bk):
    for lowidx in range(len(bk)-1):
        for highidx in range(lowidx+1, len(bk)):
            if f"{bk[highidx]}|{bk[lowidx]}" in ruleset:
                bk[highidx], bk[lowidx] = bk[lowidx], bk[highidx]

total = 0
for pages in out_of_order:
    sort_book(pages)
    total += int(pages[len(pages)//2])
print("Part 2:", total)
