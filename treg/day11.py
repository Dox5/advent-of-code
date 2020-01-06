#!/usr/bin/python

import fileinput
import intcode
import collections

def paint(hull, ins):
    pos = complex(0,0)
    mov = complex(0,1)
    painted = set()

    def outputer():
        mode = 0
        TURN = [1j, -1j]
        def o(x):
            # I don't love having to use nonlocal, but...
            nonlocal mode
            nonlocal pos
            nonlocal mov
            
            if mode == 0:
                hull[pos] = x
                painted.add(pos)
            else:
                mov *= TURN[x]
                pos += mov
            mode = (mode+1)%2

        return o

    def input():
        while True:
            yield hull[pos]
    
    cpu = intcode.CPU(ins,input, outputer())
    while not cpu.finished():
        cpu.run()

    return painted

def main():
    ins = [int(x) for x in next(fileinput.input()).split(',')]
    hull = collections.defaultdict(int)    
    painted = paint(hull, ins)
    print(len(painted))

    hull = collections.defaultdict(int)
    hull[complex(0,0)] = 1
    paint(hull, ins)
    
    minx = min(hull, key=lambda k:k.real).real
    maxx = max(hull, key=lambda k:k.real).real
    miny = min(hull, key=lambda k:k.imag).imag
    maxy = max(hull, key=lambda k:k.imag).imag
    for y in range(int(maxy), int(miny-1), -1):
        for x in range(int(minx), int(maxx+1)):
            if hull[complex(x,y)] == 1:
                print("*", end='')
            else:
                print(" ", end='')
        print()
    
if __name__ == "__main__":
    main()
