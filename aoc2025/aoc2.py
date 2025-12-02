import aoc_util

def part1(data):
    ans = set()
    for rg in data[0].strip().split(','):
        (start, end) = rg.split('-')
        # print(">range ", rg)
        start_num = int(start)
        end_num = int(end)
        if start_num > end_num:
            raise ValueError(f"{start} - {end}")
        xpart = start[0:len(start)//2]
        if xpart == '':
            xpart = '1'

        while True:
            x = int(xpart * 2)
            if x > end_num:
                break
            if start_num <= x <= end_num:
                ans.add(int(x))
            xpart = str(int(xpart) + 1)
    return sum(ans)

def part2(data):
    ans = set()
    for rg in data[0].strip().split(','):
        (start, end) = rg.split('-')
        # print(">range ", rg)
        start_num = int(start)
        end_num = int(end)
        if start_num > end_num:
            raise ValueError(f"{start} - {end}")
        for repc in range(2,len(end)+1):
            xpart = start[0:len(start)//repc]
            if xpart == '':
                xpart = '1'

            while True:
                x = int(xpart * repc)
                if x > end_num:
                    break
                if start_num <= x <= end_num:
                    ans.add(int(x))
                xpart = str(int(xpart) + 1)
    return sum(ans)

if __name__ == '__main__':
    data = aoc_util.get_data_lines(2)
    print(part1(data))
    print(part2(data))
