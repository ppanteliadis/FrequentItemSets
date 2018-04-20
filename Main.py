# Import the libraries
import random

def dbound():
    for i in range(1, 10000000):
        print("testing: " + str(i))
        found = 0
        current = []
        for t in transactions:
            if(found >= i):
                continue
            if(len(t) >= i):
                if(t not in current):
                    current.append(t)
                    found = found + 1
        if(found >= i):
            print("found d = " + str(i))
        else:
            print(" not found d = " + str(i))
            break

def avg_length():
    t = 0
    num = 0
    for x in transactions:
        t = t + len(x)
        num = num + 1

    print(t/num)



# file = open('test.txt', 'r')
file = open('kosarak.dat', 'r')
data = file.readlines()
transactions = []
for line in data:
    t = line.split()
    transactions.append(t)

dbound()
avg_length()


