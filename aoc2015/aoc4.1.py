import hashlib

secret = b'bgvyzdsv'
i = 1
while not hashlib.md5(secret + str(i).encode('ascii')).hexdigest().startswith('000000'):
    i += 1


print(i)
