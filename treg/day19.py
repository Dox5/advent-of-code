#!/usr/bin/python

import intcode
import fileinput

def printGrid(grid,mx=50,my=50):
    for y in range(my):
        for x in range(mx):
            print(grid[complex(x,y)], end='')
        print()        

def tractor(ins):
    grid = {}
    pos = None
    
    def outputter(x):
        nonlocal grid
        nonlocal pos
        if x in [0,1]:
            grid[pos] = x
        else:
            print(x, end='')

    def inputter():
        nonlocal pos
        for x in range(50):
            for y in range(50):
                pos = complex(x,y)
                yield x
                yield y
        yield None

    cpu = intcode.CPU(ins, inputter, outputter)
    try:
        while True:
            cpu.run()
            cpu.reset()
    except:
        pass

    printGrid(grid)
    
    return sum(grid.values())

def shipFinder(ins):
    pos = None
    plan = 0 # 0 'Stepping down', 1 is look right, 2 look up
    trials = [complex(0,1100)] # Picked by exploring reasonable start points
    result = None
    
    def outputter(x):
        nonlocal pos
        nonlocal plan
        nonlocal trials
        nonlocal result
        
        if plan == 0: # We were stepping down.
#            print("Step down plan was in operation")
            if x == 0: 
                # No point down here, shuffle right to find one
                trials = [trials[-1]+1]
            else:
                # Aha a point. Does santa fit horizontally
                plan = 1
                trials.append(trials[-1]+99)
        elif plan == 1:
#            print("Look right plan was in operation")
            if x == 0:
                # Not find enough, go back and look down
                plan = 0
                trials = [trials[0]+1j]
            else:
                # Wide enough. Tall enough as well
                plan = 2
                trials.append(trials[-1]-99j)
        elif plan == 2:
#            print("Look up plan was in operation")            
            if x == 0:
                # Not here, best step down instead
                plan = 0
                trials = [trials[0]+1j]
            else:
                # Success!
                result = complex(trials[0].real, trials[2].imag)
#                print("Closest point = ", result)
                trials = [None]

    def inputter():
        nonlocal trials        
        while True:
#            print("Exploring", trials[-1])
            yield trials[-1].real
            yield trials[-1].imag

    cpu = intcode.CPU(ins, inputter, outputter)
    try:
        while True:
            cpu.run()
            cpu.reset()
    except:
        pass

    return result
            
def main():
    ins = [int(x) for x in next(fileinput.input()).strip().split(',')]
    pulled = tractor(ins)
    print(pulled)
    
    fitter = shipFinder(ins)
    print((fitter.real * 10000) + fitter.imag)
    
if __name__ == "__main__":
    main()
