#!/usr/bin/python3

import fileinput
import sys
from intcode import runComputer

def part1(ins):
    p1i = list(ins)
    p1i[1] = 12
    p1i[2] = 2
    return runComputer(p1i)

def part2(ins):
    target = 19690720
    for noun in range(0, 100):
        for verb in range(0, 100):
            testi = list(ins)
            testi[1] = noun
            testi[2] = verb
            if runComputer(testi) == target:
                return (100*noun)+verb
    
if __name__ == "__main__":
    line = list(fileinput.input())[0]
    instructions = [int(x) for x in line.split(',')]
    print(part1(instructions))
    print(part2(instructions))
