#!/usr/bin/python
import collections
import fileinput
import copy

def buildGrid(rows):
    grid = collections.defaultdict(lambda: '.')
    for y in range(len(rows)):
        for x in range(len(rows[0])):
            grid[(0,complex(x,y))] = rows[y][x]
    return grid

def simpleNeighbours(grid, key, lookfor):
    (level, pos) = key
    if level != 0:
        raise Exception("Not a simple neighbour calculation!")
        
    neigh = [option+pos for option in [1+0j,-1+0j,0+1j,0-1j]]
    matched = [n for n in neigh if (0,n) in grid and grid[(0,n)] == lookfor]
    return len(matched)

def biodiversity(grid):
    bio = 0
    for k in grid.keys():
        (_,pos) = k
        power = int(pos.real+(pos.imag*5))
        if grid[(0, pos)] == '#':
            bio += 1 << power
    return bio

def evolve(grid, neighbours=simpleNeighbours):
    newgrid = collections.defaultdict(lambda: '.')    
    for k in grid.keys():
        bugCount = neighbours(grid,k,'#')

        if grid[k] == '#' and bugCount == 1:
            newgrid[k] = '#'
        elif grid[k] == '.' and bugCount in [1,2]:
            newgrid[k] = '#'
            
    return newgrid

def printgrid(grid, level=0):
    for y in range(5):
        for x in range(5):
            print(grid[(level,complex(x,y))],end='')
        print()
    print()           
    
def part1(grid):
    seen = set()
    seen.add(biodiversity(grid))
    printgrid(grid)
    while True:

        newgrid = evolve(grid) # Use default neighbour option
        printgrid(newgrid)
        bio = biodiversity(newgrid)

        if bio in seen:
            return (newgrid, bio)
        else:
            seen.add(bio)
        grid = newgrid

def main():
    rows = [x.strip() for x in fileinput.input()]
    grid = buildGrid(rows)

    (repeatedGrid, diversity) = part1(grid)
    print("Part 1:", diversity)

    # OK, part 2 is a bit fancier. Probably need a grid datastructre
    # with levels in it alongside complex coords. Neighbour calc gets
    # a touch fancier and has to check levels etc. Don't need a new
    # bio diversity calc though

    
    
if __name__ == "__main__":
    main()
