# seti 123 0 3
# bani 3 456 3
# eqri 3 72 3
# addr 3 5 5
# seti 0 0 5
# seti 0 9 3
# bori 3 65536 1
# seti 9450265 6 3
# bani 1 255 4
# addr 3 4 3
# bani 3 16777215 3
# muli 3 65899 3
# bani 3 16777215 3
# gtir 256 1 4
# addr 4 5 5
# addi 5 1 5
# seti 27 1 5
# seti 0 9 4
# addi 4 1 2
# muli 2 256 2
# gtrr 2 1 2
# addr 2 5 5
# addi 5 1 5
# seti 25 7 5
# addi 4 1 4
# seti 17 5 5
# setr 4 6 1
# seti 7 8 5
# eqrr 3 0 4
# addr 4 5 5
# seti 5 8 5

reg0 = 0
reg1 = 0
reg2 = 0
reg3 = 0
reg4 = 0
seen = set([])
while True:
    reg1 = reg3 | 65536
    reg3 = 9450265
    while True:
        reg4 = reg1 & 0xFF
        reg3 += reg4
        reg3 &= 0xFFFFFF
        reg3 *= 65899
        reg3 &= 0xFFFFFF
        if 256 > reg1:
            break
        reg4 = 0
        while True:
            if 256*(reg4+1) > reg1:
                reg1 = reg4
                break
            reg4 += 1
            #print(f"ip=24 [0, {reg1}, 0, {reg3}, {reg4-1}, 24]")
    print(reg3)
    if reg3 in seen:
        break
    seen.add(reg3)
