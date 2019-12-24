#!/usr/bin/python
import collections
import fileinput
import copy

def buildGrid(rows):
    grid = collections.defaultdict(lambda: '.')
    for y in range(len(rows)):
        for x in range(len(rows[0])):
            grid[complex(x,y)] = rows[y][x]
    return grid

def neighbours(grid, pos, lookfor):
    neigh = [option+pos for option in [1+0j,-1+0j,0+1j,0-1j]]
    matched = [n for n in neigh if grid[n] == lookfor]
    return len(matched)

def biodiversity(grid):
    bio = 0
    for y in range(5):
        for x in range(5):
            power = x+(y*5)
            if grid[complex(x,y)] == '#':
                bio += 1 << power
    return bio

def evolve(grid):
    newgrid = collections.defaultdict(lambda: '.')    
    for x in range(5):
        for y in range(5):
            pos = complex(x,y)
            bugCount = neighbours(grid,pos,'#')
            if grid[pos] == '#' and bugCount == 1:
                newgrid[pos] = '#'
            elif grid[pos] == '.' and bugCount in [1,2]:
                newgrid[pos] = '#'
    return newgrid

def printgrid(grid, level=-1):
    for y in range(5):
        for x in range(5):
            if level == -1:
                print(grid[complex(x,y)],end='')
            else:
                print(grid[(level,complex(x,y))],end='')
        print()
    print()           
    
def part1(grid):
    seen = set()
    seen.add(biodiversity(grid))

    while True:
        newgrid = evolve(grid)
        bio = biodiversity(newgrid)

        if bio in seen:
            return (newgrid, bio)
        else:
            seen.add(bio)
        grid = newgrid

def remapIntoLevel(grid):
    newgrid = collections.defaultdict(lambda: '.')
    for k,v in grid.items():
        newgrid[(0,k)] = v
    return newgrid
        
def main():
    rows = [x.strip() for x in fileinput.input()]
    grid = buildGrid(rows)

    (repeatedGrid, diversity) = part1(grid)
    print("Part 1:", diversity)

    # OK, part 2 is a bit fancier. Probably need a grid datastructre
    # with levels in it alongside complex coords. Neighbour calc gets
    # a touch fancier and has to check levels etc. Don't need a new
    # bio diversity calc though
    grid = remapIntoLevel(grid)
    
    
if __name__ == "__main__":
    main()
