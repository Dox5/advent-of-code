#!/usr/bin/python3

import fileinput

def buildGrids(wires):
    grids = []
    delta = {'U': (0,1), 'R': (1,0), 'D': (0,-1), 'L': (-1, 0)}    

    for wire in wires:
        grid  = {}
        pos   = (0,0)
        steps = 0
        moves = wire.split(',')
        for move in moves:
            spaces = int(move[1:])
            direction = delta[move[0]]
            for i in range(spaces):
                steps += 1
                pos = tuple(x+y for x,y in zip(pos,direction))
                if pos not in grid:
                    grid[pos] = steps
        grids.append(grid)
    return grids

def crossovers(grids):
    crossovers = grids[0].keys()
    for grid in grids[1:]:
        crossovers = crossovers & grid.keys()
    return crossovers

def nearestCross(crossovers):
    return min([abs(x)+abs(y) for x,y in crossovers])

def shortCircuit(crossovers, grids):
    return min([sum([element[x] for element in grids]) for x in crossovers])

def main():
    wires = fileinput.input()    
    grids = buildGrids(wires)
    cross = crossovers(grids)
    print(nearestCross(cross))
    print(shortCircuit(cross,grids))
               
if __name__ == "__main__":
    main()
