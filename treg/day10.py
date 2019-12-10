#!/usr/bin/python
import fileinput

def examine(grid, xy, xdyd):
    x,y = xy
    xd, yd = xdyd
    found = 0
    for dirmod in [[1,1],[-1,1],[1,-1],[-1,-1]]:
        if xd == 0 and dirmod[0] == -1:
            continue
        if yd == 0 and dirmod[1] == -1:
            continue
        direction = tuple([a*b for a,b in zip([xd,yd],dirmod)])
        nx,ny = x,y
        try:
            d = False
            while not d:
                nx,ny = tuple([a+b for a,b in zip([nx,ny],direction)])
                if nx >= 0 and ny >= 0:
                    if grid[nx][ny] == '#':
                        found += 1
                        d = True
                else:
                    raise()    
        except:
            pass
    return found

def part1(grid):
    # OK, need to find every asteroid and then play prime numbered
    # jumping games
    maxx = len(grid)
    maxy = len(grid[0])

    options = [(x,y) for x in range(maxx) for y in range(maxy) if x != 0 or y != 0]
    print(options)
    # Some clever sort by gradient function needed for part 2
    
    for (x,y) in options:
        for m in range(2,max(maxx,maxy)):
            nx,ny = tuple([a*b for a,b in zip((x,y),(m,m))])
            try:
                options.remove((nx,ny))
            except:
                pass
                
    sightings = {}
    
    for x in range(maxx):
        for y in range(maxy):
            if grid[x][y] == '#':
                inSight = 0
                for opt in options:
                    inSight += examine(grid,(x,y),opt)
                sightings[(x,y)] = inSight

    bestasteroid = max(sightings, key=lambda k: sightings[k])
    return (bestasteroid, sightings[bestasteroid])

def part2(grid):
    pass

def main():
    # INDEXED Y,X!!!
    lines = [l.strip() for l in fileinput.input()]

    # Indexed X,Y!!
    grid = [[line[x] for line in lines] for x in range(len(lines))]

    # Loop over asteroids
    print(part1(grid))
    
if __name__ == "__main__":
    main()


    
