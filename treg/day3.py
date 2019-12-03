#!/usr/bin/python3

import fileinput
from collections import defaultdict

grid = defaultdict(list)
delta = {'U': (0,1), 'R': (1,0), 'D': (0,-1), 'L': (-1, 0)}

def distance(a,b):
    return abs(a[0]-b[0])+abs(a[1]-b[1])

def main():
    wires = fileinput.input()    
    for idx, wire in enumerate(wires):
        pos = (0,0)
        steps = 0
        grid[pos].append((idx,steps))
        moves = wire.split(',')
        for move in moves:
            spaces = int(move[1:])
            direction = delta[move[0]]
            for i in range(spaces):
                steps += 1
                pos = tuple(x+y for x,y in zip(pos, direction))
                grid[pos].append((idx,steps))    


    #Part 1
    crosses = [(key, distance((0,0),key)) for key,val in grid.items() if len(val) > 1 and key != (0,0) and len(set([x for x,y in val])) == 2]
    
    def bySecondElement(elem):
        return elem[1]

    crosses.sort(key=bySecondElement)
    print(crosses[0][1])

    #Part 2
    def cycles(a):
        wirea = [y for x,y in a if x == 0]
        wireb = [y for x,y in a if x == 1]
        return wirea[0] + wireb[0]
        
    crosses = [(key, cycles(val)) for key, val in grid.items() if len(val) > 1 and key != (0,0) and len(set([x for x,y in val])) == 2]
    crosses.sort(key=bySecondElement)
    print(crosses[0][1])
    
if __name__ == "__main__":
    main()
