import re
import sys
import string

input_data = open('input_2.txt' if len(sys.argv) < 2 else sys.argv[1])
input_lines = input_data.read().splitlines()

match_2 = re.compile('^(' + '|'.join(f"[^{c}]*{c}[^{c}]*{c}[^{c}]*" for c in string.ascii_lowercase) + ')$')
match_3 = re.compile('^(' + '|'.join(f"[^{c}]*{c}[^{c}]*{c}[^{c}]*{c}[^{c}]*" for c in string.ascii_lowercase) + ')$')
counter2 = sum(1 for ln in input_lines if match_2.match(ln))
counter3 = sum(1 for ln in input_lines if match_3.match(ln))

print('input_lines counter:' + str(input_lines.__len__()))
print('counter3: ' + str(counter3))
print('counter2: ' + str(counter2))
print('checksum23: ' + str(counter2 * counter3))

