#!/usr/bin/python3

import fileinput
from intcode import CPU

def part1(ins):
    outs = []
    p1i = lambda : (x for x in [1])
    p1o = lambda x : outs.append(x)        
        
    cpu = CPU(ins,p1i,p1o)
    cpu.run()
    return outs[-1]

def part2(ins):
    outs = []
    p2i = lambda : (x for x in [5])
    p2o = lambda x : outs.append(x)

    cpu = CPU(ins,p2i,p2o)
    cpu.run()
    return outs[-1]
    
def main():
    line = list(fileinput.input())[0]
    instructions = [int(x) for x in line.split(',')]
    print(part1(instructions))
    print(part2(instructions))

if __name__ == "__main__":
    main()
