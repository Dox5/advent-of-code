#!/usr/bin/python

import fileinput
import itertools

base = [0,1,0,-1]

def part1(signal, phases=100):    
    res = [x for x in signal]
    for p in range(phases):
        newres = []
        for i in range(len(res)):
            parts = [itertools.repeat(base[x], i+1) for x in range(len(base))]
            pattern = itertools.cycle(itertools.chain(*parts))
            next(pattern)
            pairs = [[int(x),y] for x,y in zip(res,pattern)]
            mulpairs = [x[0]*x[1] for x in pairs]
            sumpairs = sum(mulpairs)
            newres.append(str(sumpairs)[-1])
        res = list(newres)
    return ''.join(res[0:8])

def part2(signal, phases=100):
    # For any given character position, all previous values must be multiplied by 0 (base expanation
    # causes triangle pattern in examples). So we don't need to think about them.
    #
    # For any character position in the second half of the string, not only are all previous positions
    # mutlipied by 0 (above), but all ones ahead are only multiple by 1 (base expantion again)
    #
    # Working backwards from the end to the mid point then is just a cumulative sum.
    #
    # Examples and input have a message start point further in than the mid point.

    bigsignal = ''.join([x for x in itertools.repeat(signal, 10000)])
    siglen = len(bigsignal)
    sigpoint = int(bigsignal[0:7])
    if sigpoint < siglen/2:
        raise("Cumulative Sum won't work as idx is less than half way in")

    interesting = [int(x) for x in bigsignal[sigpoint:]]
    for p in range(phases):
        cumulative = 0
        for c in range(len(interesting)-1, -1, -1):
            cumulative = (cumulative + interesting[c]) % 10
            interesting[c] = cumulative
    return ''.join([str(x) for x in interesting[0:8]])
    
def main():
    signal = next(fileinput.input()).strip()    
    print(part1(signal,100))
    print(part2(signal,100))
            
if __name__ == "__main__":
    main()
