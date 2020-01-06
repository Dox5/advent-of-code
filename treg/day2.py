#!/usr/bin/python3

import fileinput
from intcode import CPU

def part1(ins):
    p1i = list(ins)
    p1i[1] = 12
    p1i[2] = 2
    cpu = CPU(p1i)
    cpu.run()
    return cpu.memory[0]

def part2(ins):
    target = 19690720
    testi = list(ins)
    cpu = CPU(testi)
    
    for noun in range(0, 100):
        for verb in range(0, 100):
            cpu.memory[1] = noun
            cpu.memory[2] = verb
            cpu.run()
            if cpu.memory[0] == target:
                return (100*noun)+verb
            else:
                cpu.reset()
    
if __name__ == "__main__":
    line = list(fileinput.input())[0]
    instructions = [int(x) for x in line.split(',')]
    print(part1(instructions))
    print(part2(instructions))
