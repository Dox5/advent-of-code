#!/usr/bin/python

import fileinput
from intcode import CPU

def solver(ins,init):
    out = []
    output = lambda x: out.append(x)
    inputs = lambda: (x for x in [init])
    c = CPU(ins,inputs,output)
    c.run()
    return out
    
def main():
    line = next(fileinput.input()).strip()
    ins = [int(x) for x in line.split(',')]
    print(solver(ins,1))
    print(solver(ins,2))
    
if __name__ == "__main__":
    main()
