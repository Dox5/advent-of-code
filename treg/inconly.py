#!/usr/bin/python3

from collections import Counter
from day4 import validateGrouping

class IncInt:        
    def __init__(self, number, ru=True):
        self.num = str(number)
        if ru:
            self.roundUp()
        else:
            self.roundDown()

    def roundUp(self):
        workedOn = self.num[0]
        for l in self.num[1:]:
            if l < workedOn[-1]:
                workedOn += (workedOn[-1] * (len(self.num)-len(workedOn)))
                break
            else:
                workedOn += l
        self.num = workedOn

    def roundDown(self):
        workedOn = self.num[0]
        for l in self.num[1:]:
            if l < workedOn[-1]:
                workedOn = workedOn[0:-1] + chr(ord(workedOn[-1])-1)
                workedOn += '9' * (len(self.num) - len(workedOn))
                self.num = workedOn
                return self.roundDown()
            else:
                workedOn += l
        self.num = workedOn
            
    def __str__(self):
        return self.num

    def inc(self):
        self.num = str(int(self.num)+1)
        self.roundUp()

    def dec(self):
        self.num = str(int(self.num)-1)
        self.roundDown()

    def __eq__(self, other):
        return self.num == other.num

    def __ne__(self, other):
        return not (self == other)
    
    def __gt__(self, other):
        return int(self.num) > int(other.num)

    def __lt__(self, other):
        return int(self.num) < int(other.num)

    def __le__(self, other):
        return int(self.num) <= int(other.num)

    def __ge__(self, other):
        return int(self.num) >= int(other.num)
    
import operator
a = IncInt(172851)
b = IncInt(675869, False)
x = []
y = []

while a <= b:
    cnt = Counter()
    cnt.update(str(a))
    x.append(validateGrouping(cnt, operator.ge))
    y.append(validateGrouping(cnt, operator.eq))
    a.inc()

print(sum(x))
print(sum(y))

