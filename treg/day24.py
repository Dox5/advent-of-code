#!/usr/bin/python
import collections
import fileinput
import copy

def buildGrid(rows):
    grid = {}
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

def fancyNeighbours(grid, key, lookfor):
    (level, pos) = key
    # Three types of cell we can find.
    # 1: x == 0 || y == 0 || x == 2 || y == 2 (An outer edge)
    #  Neighbours include level up tiles
    # 2: (2,1), (1,2), (3,2), (3,3) - Inner edges
    #  Neighbours include all relevant edges of layer below
    # 3: Everything else
    #  Normal calc as before
    if pos.real == 0 or pos.real == 4 or pos.imag == 0 or pos.imag == 4:
        # Outer edge
        neigh = [option+pos for option in [1+0j,-1+0j,0+1j,0-1j]]
        neigh = [(level,p) for p in neigh if 0 <= p.real < 5 and 0 <= p.imag < 5]
        
        if pos.real == 0:
            neigh += [(level+1,complex(1,2))]
        elif pos.real == 4:
            neigh += [(level+1,complex(3,2))]
        if pos.imag == 0:
            neigh += [(level+1,complex(2,1))]
        elif pos.imag == 4:
            neigh += [(level+1,complex(2,3))]

    elif pos in [(1+1j),(3+1j),(1+3j),(3+3j)]:
        # Normal
        neigh = [(level,option+pos) for option in [1+0j,-1+0j,0+1j,0-1j]]
    else:
        # Inner edge
        neigh = [option+pos for option in [1+0j,-1+0j,0+1j,0-1j]]
        neigh = [(level,p) for p in neigh if not (p.real == 2 and p.imag == 2)]

        if pos == 2+1j:
            neigh += [(level-1,complex(p,0)) for p in range(5)]
        elif pos == 1+2j:
            neigh += [(level-1,complex(0,p)) for p in range(5)]
        elif pos == 3+2j:
            neigh += [(level-1,complex(4,p)) for p in range(5)]
        elif pos == 2+3j:
            neigh += [(level-1,complex(p,4)) for p in range(5)]

    # OK, we have all the neighbours. Ask about lookfor values
    matched = [n for n in neigh if n in grid and grid[n] == lookfor]
    return len(matched)
        
def biodiversity(grid):
    bio = 0
    for k in grid.keys():
        (_,pos) = k
        power = int(pos.real+(pos.imag*5))
        if grid[(0, pos)] == '#':
            bio += 1 << power
    return bio

def manipulateFloors(grid):
    # Figure out min/max floors at present
    floors = [f for (f,_) in grid.keys()]
    minf = min(floors)
    maxf = max(floors)

    for floor,change in [(minf,-1),(maxf,+1)]:
        items = sum([v == '#' for k,v in grid.items() if k[0] == floor])
        if items > 0:
            for y in range(5):
                for x in range(5):
                    if not (x == 2 and y == 2):
                        grid[(floor+change,complex(x,y))] = '.'

    return grid
    
def evolve(grid, neighbours=simpleNeighbours):
    newgrid = {}

    if neighbours != simpleNeighbours:
        grid = manipulateFloors(grid) # Worry about higher and lower floors that may need adjusting
        
    for k in grid.keys():
        bugCount = neighbours(grid,k,'#')

        if grid[k] == '#':
            if bugCount == 1:
                newgrid[k] = '#'
            else:
                newgrid[k] = '.'
        elif grid[k] == '.':
            if bugCount in [1,2]:
                newgrid[k] = '#'
            else:
                newgrid[k] = '.'
            
    return newgrid

def printgrid(grid, level=0):
    for y in range(5):
        for x in range(5):
            if (level,complex(x,y)) in grid:
                print(grid[(level,complex(x,y))],end='')
            else:
                print('X', end='')
        print()
    print()           
    
def part1(grid):
    seen = set()
    seen.add(biodiversity(grid))
    while True:
        newgrid = evolve(grid) # Use default neighbour option
        bio = biodiversity(newgrid)        

        if bio in seen:
            return (newgrid, bio)
        else:
            seen.add(bio)
        grid = newgrid

def part2(grid, iterations=200):
    newgrid = grid
    for i in range(1,iterations+1):
        print("Processing Iteration", i)
        newgrid = evolve(newgrid, fancyNeighbours)        
    return sum(value == '#' for value in newgrid.values())   
        
def main():
    rows = [x.strip() for x in fileinput.input()]
    grid = buildGrid(rows)

    (repeatedGrid, diversity) = part1(grid)
    print("Part 1:", diversity)

    # OK, part 2 is a bit fancier. Probably need a grid datastructre
    # with levels in it alongside complex coords. Neighbour calc gets
    # a touch fancier and has to check levels etc. Don't need a new
    # bio diversity calc though.

    # The centre cell is also now not really a thing, so perhaps remove
    # that so we don't iterate across it
    grid.pop((0,complex(2,2)))
    print("Part 2:", part2(grid))

    
    
if __name__ == "__main__":
    main()
