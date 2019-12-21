#!/usr/bin/python
import intcode
import fileinput
import string

PRINTABLE = string.ascii_uppercase + string.ascii_lowercase + ''.join(['.','@','#',' ',':','\'',chr(10)])

def runner():
    # As for part A, but also make sure we can either step forward or immediately jump again
    for c in "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nNOT E T\nNOT T T\n OR H T\nAND T J\n":
        yield ord(c)

    for c in "RUN\n":
        yield ord(c)

def walker():
    # Any of upcoming three are holes, but D is OK
    for c in "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\n":
        yield ord(c)
        
    for c in "WALK\n":
        yield ord(c)

def spring(ins, command):
    def outputter(x):
        if x <= 127:
            if chr(x) in PRINTABLE:
                print(chr(x), end='')
            else:
                raise Exception("Unexpected chr")
        else:
            print("End of path:",x)
        
    cpu = intcode.CPU(ins, command, outputter)
    cpu.run()
    
def main():
    ins = [int(x) for x in next(fileinput.input()).strip().split(',')]
    spring(ins, walker)
    spring(ins, runner)
    
if __name__ == "__main__":
    main()
