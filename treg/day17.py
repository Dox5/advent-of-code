#!/usr/bin/python

import networkx
import fileinput
import collections
import intcode

def scafold(ins):
    grid = collections.defaultdict(lambda: '.')
    pos = 0+0j
    robo = None
    
    def outputter(s):
        nonlocal pos
        nonlocal grid
        nonlocal robo

        if s == 10:
            pos = complex(0, pos.imag-1)
        else:
            grid[pos] = chr(s)
            if s in [ord('<'), ord('>'), ord('^'), ord('v')]:
                robo = pos
            pos += 1+0j

    cpu = intcode.CPU(ins, None, outputter)
    cpu.run()
    return (robo, grid)            

def printGrid(layout):
    mx = int(max([x.real for x in layout.keys()]))
    my = int(max([abs(x.imag) for x in layout.keys()]))
    for y in range(0,-my-1,-1):
        for x in range(mx+1):
            print(layout[complex(x,y)], end='')
        print()
        
def crossover(layout):
    crossSum = 0
    mx = int(max([x.real for x in layout.keys()]))
    my = int(max([abs(x.imag) for x in layout.keys()]))
    for y in range(0,-my-1,-1):
        for x in range(mx+1):
            if layout[complex(x,y)] == layout[complex(x+1,y)] == layout[complex(x-1,y)] == layout[complex(x,y-1)] == layout[complex(x,y+1)] == '#':
                crossSum += (x * abs(y))

    return crossSum

def surroundings(robo):
    return [complex(robo.real-1,robo.imag),complex(robo.real+1,robo.imag),complex(robo.real,robo.imag+1),complex(robo.real,robo.imag-1)]

def roboFace(robo):
    if robo == '<':
        return -1+0j
    elif robo == '>':
        return 1+0j
    elif robo == 'v':
        return 0-1j
    elif robo == '^':
        return 0+1j

def behind(rf):
    return rf * -1       

def selectTurn(rf, facings):
    if rf in facings or len(facings) == 0:
        return 1
    else:
        return facings[0] / rf

def findFullPath(robo, grid):
    # Track
    distance = 0
    instructions = []
    
    # Which way are we facing
    rf = roboFace(grid[robo])

    while grid[robo] != '.':
        # Get our surroundings
        facings = [x-robo for x in surroundings(robo) if grid[x] == '#']

        # Drop the option that is behind us
        try:
            facings.remove(behind(rf))
        except ValueError:
            pass
        
        # Choose turn option
        turn = selectTurn(rf, facings)
        if turn == 1:
            # Straight ahead, increase distance
            distance += 1
            robo += rf
        else:
            # Reached a turn, add the instructions for distance travelled if > 0 and our turn
            if distance > 0:
                instructions.append(str(distance))
                distance = 0
            if turn == -1j:
                instructions.append("R")
            else:
                instructions.append("L")
            rf *= turn

    # Catch the move out of bounds
    instructions.append(str(distance-1))
    return instructions

def reduceInstructions(ins):
    programs = []
    order = []
    start = 0
    for i in ['A','B','C']:
        chunk = 1        
        trialsub = []
        matches = []
        lastmatches = []
        while True:
            trialsub = ins[start:start+chunk]
            if any((True for x in ["A","B","C"] if x in trialsub)):
                # Hit program
                break

            matches = [x==y for x,y in [[ins[pos:pos+len(trialsub)], trialsub] for pos in range(start+1,len(ins)-len(trialsub)+1)]]
            if any(matches):
                # Trial sub exists, try bigger
                chunk += 1
                lastmatches = matches[:]                                      
            else:
                # Too big!
                break

        programs.append(','.join(ins[start:start+(chunk-1)]))

        # Now replace
        curins = ','.join(ins)
        curins = curins.replace(programs[-1], i)
        ins = curins.split(',')
        for start in range(len(ins)):
            if ins[start] not in ['A','B','C']:
                break

    return (curins, programs)

def saveTheRobots(ins, order, programs):
    ins[0] = 2
    result = None
    
    def inputter():
        nonlocal order
        nonlocal programs

        # Add newline to order and submit
        order += chr(10)
        for c in order:
            yield ord(c)

        for p in programs:
            p += chr(10) # Newline delimit
            for c in p:
                yield ord(c)

        # Video request - no thanks
        yield ord('n')
        yield 10

    def outputter(response):
        nonlocal result
        result = response
        
    cpu = intcode.CPU(ins, inputter, outputter)
    cpu.run()
    return result

def main():
    ins = [int(x) for x in next(fileinput.input()).strip().split(',')]
    layout = scafold(ins)
    printGrid(layout[1])        
    print(crossover(layout[1]))

    # First generate a path all the way around I guess.
    # If you can go through intersections, do so   
    moves = findFullPath(*layout)

    # Now we need to simplify into 3 programs
    (order, programs) = reduceInstructions(moves)

    # Now program our robot
    print(saveTheRobots(ins, order, programs))
    
if __name__ == "__main__":
    main()
