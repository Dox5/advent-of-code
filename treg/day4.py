#!/usr/bin/python3

from collections import Counter

minimum = 172851
maximum = 675869

def main():
    count = 0
    count2 = 0
    for password in range(minimum, maximum):
        cnt = Counter()
        strPass = str(password)
        incCheck = ''.join(sorted(strPass))
        repCheck = set(strPass)
        cnt.update(strPass)

        if strPass == incCheck:    
            # Part 1
            if len(repCheck) < len(strPass):
                count = count + 1

            # Part 2
            if len([e for e in cnt.elements() if cnt[e] == 2]):
                count2 = count2 + 1
            
    print(count)
    print(count2)
            
if __name__ == "__main__":
    main()
