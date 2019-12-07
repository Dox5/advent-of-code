#!/usr/bin/python
import fileinput
from intcode import CPU
from itertools import permutations
import operator

AMP_STAGES = 5

def run(ins, perms):
    def inputBuilder(phase, loc):
        def input():
            yield phase
            while True:
                val = connections[loc]
                connections[loc] = None
                yield val               
        return input

    def outputBuilder(loc):
        def output(x):
            connections[(loc+1)%AMP_STAGES] = x
        return output

    best = {}
        
    for p in perms:
        connections = [0,None,None,None,None]        
        inputs = [inputBuilder(phase,pos) for phase,pos in zip(p,range(AMP_STAGES))]
        outputs = [outputBuilder(pos) for pos in range(AMP_STAGES)]
        cpus = [CPU(ins,inputs[x],outputs[x]) for x in range(AMP_STAGES)]

        while not all([cpu.finished() for cpu in cpus]):
            for c in cpus:
                c.run()

        best[p] = connections[0]

    bestperm = max(best, key=lambda k: best[k])
    return (bestperm, best[bestperm])

def main():
    line = list(fileinput.input())[0].strip()
    instructions = [int(x) for x in line.split(',')]
    print(run(instructions, permutations(range(AMP_STAGES))))
    print(run(instructions, permutations(range(AMP_STAGES,AMP_STAGES*2))))    
    
if __name__ == "__main__":
    main()
