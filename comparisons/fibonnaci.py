a = 1
b = 1
c = 1

for i in range(90):
    c = a
    a += b
    b = c
    print(a)
