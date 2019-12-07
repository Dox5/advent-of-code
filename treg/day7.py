#!/usr/bin/python
import fileinput
from intcode import CPU, AwaitingInput
from itertools import permutations

def run(ins, perms):
    connections = [0,None,None,None,None]
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
            connections[(loc+1)%5] = x
        return output

    best = 0
        
    for p in perms:
        connections = [0,None,None,None,None]        
        inputs = [inputBuilder(phase,pos) for phase,pos in zip(p,range(5))]
        outputs = [outputBuilder(pos) for pos in range(5)]
        cpus = [CPU(ins,inputs[x],outputs[x]) for x in range(5)]

        pendingInput = 1
        while pendingInput != 0:
            pendingInput = 0            
            for c in cpus:
                try:
                    c.run()
                except AwaitingInput as e:
                    pendingInput += 1

        if connections[0] > best:
            best = connections[0]

    return best

def main():
    line = list(fileinput.input())[0].strip()
    instructions = [int(x) for x in line.split(',')]
    print(run(instructions, permutations(range(5))))
    print(run(instructions, permutations(range(5,10))))    
    
if __name__ == "__main__":
    main()
