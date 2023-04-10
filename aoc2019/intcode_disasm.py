import sys


class IntCodeDisasm():
    def __init__(self, iprog):
        self.prog = list(iprog)

    def param_string(self, spot, param_num):
        imode = self.prog[spot] // (10 * 10 ** param_num)
        val = self.prog[spot + param_num]
        if imode % 10 == 0:
            return '[%s]' % (val,)
        if imode % 10 == 1:
            return str(val)
        if imode % 10 == 2:
            return '[rel%s%s]' % ('-' if val < 0 else '+', abs(val))
        print("BAD get imode %d at idx %d" % (imode, spot))
        sys.exit(2)

    def decode_instruction(self, spot):
        code = self.prog[spot] % 100
        if code == 1:
            return ([spot+4], '%s <- %s + %s' % (self.param_string(spot, 3),
                                                 self.param_string(spot, 1),
                                                 self.param_string(spot, 2)),
                    frozenset(range(spot, spot+4)))
        elif code == 2:
            return ([spot+4], '%s <- %s * %s' % (self.param_string(spot, 3),
                                                 self.param_string(spot, 1),
                                                 self.param_string(spot, 2)),
                    frozenset(range(spot, spot+4)))
        elif code == 3:
            return ([spot+2], '%s <- input' % (self.param_string(spot, 1),),
                    frozenset(range(spot, spot+2)))
        elif code == 4:
            return ([spot+2], 'output %s' % (self.param_string(spot, 1),),
                    frozenset(range(spot, spot+2)))
        elif code == 5:
            p1 = self.param_string(spot, 1)
            p2 = self.param_string(spot, 2)
            spots = [spot+3]
            if '[' not in p2:
                spots.append(int(p2))
            if p1 == '1':
                del spots[0]
            return (spots, 'if %s then goto %s' % (p1, p2),
                    frozenset(range(spot, spot+3)))
        elif code == 6:
            p1 = self.param_string(spot, 1)
            p2 = self.param_string(spot, 2)
            spots = [spot+3]
            if '[' not in p2:
                spots.append(int(p2))
            if p1 == '0':
                del spots[0]
            return (spots, 'if not %s then goto %s' % (p1, p2),
                    frozenset(range(spot, spot+3)))
        elif code == 7:
            p1 = self.param_string(spot, 1)
            p2 = self.param_string(spot, 2)
            p3 = self.param_string(spot, 3)
            return ([spot+4], '%s <- (%s < %s)' % (p3, p1, p2),
                    frozenset(range(spot, spot+4)))
        elif code == 8:
            p1 = self.param_string(spot, 1)
            p2 = self.param_string(spot, 2)
            p3 = self.param_string(spot, 3)
            return ([spot+4], '%s <- (%s == %s)' % (p3, p1, p2),
                    frozenset(range(spot, spot+4)))
        elif code == 9:
            p1 = self.param_string(spot, 1)
            return ([spot+2], 'rel %s%s' % ('+' if int(p1) > 0 else '', p1),
                    frozenset(range(spot, spot+2)))
        elif code == 99:
            return ([], 'halt', frozenset([spot]))
        else:
            return ([], None, frozenset())

    def decode(self, start_ips=(0,)):
        ip_list = list(start_ips)
        decode = {}
        code_ips = set()
        while ip_list:
            ip = ip_list.pop()
            if ip not in decode:
                (more_ip, txt, codebits) = self.decode_instruction(ip)
                code_ips.update(codebits)
                if txt is not None:
                    decode[ip] = txt
                ip_list.extend(more_ip)
        for ip in range(len(self.prog)):
            if ip in decode:
                print('%5d: %s' % (ip, decode[ip]))
            elif ip not in code_ips:
                print('%5d: data %s' % (ip, self.prog[ip]))


with open('prog.in' if len(sys.argv) < 2 else sys.argv[1]) as f:
    data = list(f)

for line in data:
    prog = [int(x) for x in line.split(',')]

if len(sys.argv) > 2:
    start_ips = [int(x) for x in sys.argv[2:]]
else:
    start_ips = [0]
IntCodeDisasm(prog).decode(start_ips)
