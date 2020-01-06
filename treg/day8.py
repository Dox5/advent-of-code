#!/usr/bin/python
import fileinput
import sys
import collections

WIDTH = 25
HEIGHT = 6

BLACK = 0
WHITE = 1
TRANS = 2

def part1(layers):
    layerInfo = {}
    for idx, layer in enumerate(layers):
        c = collections.Counter(layer)
        layerInfo[idx] = c

    mzi = min(layerInfo, key=lambda k: layerInfo[k][0])
    return layerInfo[mzi][1] * layerInfo[mzi][2]

def toSinglePixel(stack):
    while stack[0] == TRANS:
        stack.pop(0)
    return stack[0]

def part2(layers):
    pixStacks = [[layer[x] for layer in layers] for x in range(PIXELS)]
    pix = [toSinglePixel(s) for s in pixStacks]

    for i,p in enumerate(pix):
        if i % WIDTH == 0:
            print()
        if p == BLACK:
            print(" ", end='')
        else:
            print("*", end='')       

def main():
    nums = next(fileinput.input(sys.argv[1])).strip()
    vals = [int(x) for x in nums]
    layers = [vals[x:x+PIXELS] for x in range(0,len(vals),PIXELS)]
    print(part1(layers))
    part2(layers)
    
if __name__ == "__main__":
    if len(sys.argv) > 2:
        WIDTH = int(sys.argv[2])
        HEIGHT = int(sys.argv[3])

    PIXELS = WIDTH*HEIGHT        
    main()
