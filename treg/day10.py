#!/usr/bin/python

import fileinput
import math
import cmath

def lookaround(field,loc,vectors):
    seen = 0
    for v in vectors:
        try:
            pos = loc
            while True:
                pos = pos + v
                if field[pos] == '#':
                    seen += 1
                    break                
        except:
            pass    
    return seen
        
def genVectors(dim):
    # Create vectors in top right space
    vectors = set()
    for x in range(dim):
        for y in range(1,dim):
            div = math.gcd(x,y)
            vectors.add(complex(x/div,y/div))

    # Sort vectors by phase descending for part 2 ease
    vectors = sorted([v for v in vectors], key=lambda x: cmath.phase(x), reverse=True)
            
    # Create other vectors in 360 degrees
    rotate = [-1,-1j,+1j]
    rotvec = [[x*y for x in vectors] for y in rotate]            
    for vec in rotvec:
        vectors += vec

    # Nasty hack to think about - y is inverted vis a vis normal
    spunvec = []
    for v in vectors:
        spunvec += [complex(v.real, -v.imag)]

    return spunvec

def part1(field, vectors):
    # Now look for asteroids
    asteroids = {}
    for key,val in field.items():
        if val == '#':
            asteroids[key] = lookaround(field,key,vectors)

    # Get best
    bestasteroid = max(asteroids, key=lambda k: asteroids[k])
    return (bestasteroid, asteroids[bestasteroid])

def part2(field, vectors, loc,numDest=200):
    destroyed = 0
    while True:
        for v in vectors:
            try:
                pos = loc
                while True:
                    pos = pos + v
                    if field[pos] == '#':
                        field[pos] = destroyed = destroyed + 1
                        if destroyed == numDest:
                            return pos
                        break
            except:
                pass            

def genField(lines):
    grid = [[line[x] for line in lines] for x in range(len(lines[0]))]

    maxx = len(lines[0])
    maxy = len(lines)

    field = {coord:val for (coord,val) in [(complex(x,y),grid[x][y]) for x in range(maxx) for y in range(maxy)]}
    return(field,maxx,maxy)
            
def main():
    lines = [l.strip() for l in fileinput.input()]
    (field,maxx,maxy) = genField(lines)
    vectors = genVectors(max(maxx,maxy))

    # Part 1
    (ba, num) = part1(field, vectors)
    print(ba, num)

    # Part 2
    lastasteroid = part2(field, vectors, ba)
    print(int((lastasteroid.real * 100) + lastasteroid.imag))

if __name__ == "__main__":
    main()
