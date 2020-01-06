#!/usr/bin/python3

from collections import Counter
import operator

minimum = 172851
maximum = 675869

def validateGrouping(cnts, oper):
    return len([e for e in cnts.elements() if oper(cnts[e], 2)]) > 0

def main():
    counta = []
    countb = []
    for password in range(minimum, maximum):
        cnt = Counter()
        strPass = str(password)
        incCheck = ''.join(sorted(strPass))
        
        if strPass == incCheck:    
            cnt.update(strPass)

            # Part 1            
            counta.append(validateGrouping(cnt, operator.ge))

            # Part 2
            countb.append(validateGrouping(cnt, operator.eq))
            
    print(sum(counta))
    print(sum(countb))
            
if __name__ == "__main__":
    main()
