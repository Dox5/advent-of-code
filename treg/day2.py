#!/usr/bin/python3

import fileinput
import sys

def add(ins, i):
    ins[ins[i+3]] = ins[ins[i+1]] + ins[ins[i+2]]

def mul(ins, i):
    ins[ins[i+3]] = ins[ins[i+1]] * ins[ins[i+2]]    

def runComputer(ins):
    ip = 0
    ops = {1 : add, 2 : mul}
    
    while (oper := ins[ip]) != 99:
        ops[oper](ins, ip)
        ip += 4

    return ins[0]

def part1(ins):
    p1i = list(ins)
    p1i[1] = 12
    p1i[2] = 2
    return runComputer(p1i)

def part2(ins):
    target = 19690720
    for noun in range(0, 101):
        for verb in range(0, 101):
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
