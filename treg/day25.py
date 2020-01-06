#!/usr/bin/python
import intcode
import fileinput
import itertools

def runAdventure(ins):
    def outputter(x):
        if x < 128:
            print(chr(x),end='')
        else:
            raise Exception("NotASCII")

    def inputter():
        commands = ["east","east","take fuel cell", "west", "take jam", "south", "take shell", "north", "west", "south", "west", "take easter egg", "north", "east", "take space heater", "west", "south", "west", "west", "take monolith", "south", "west", "north", "take coin", "south", "east", "north", "west", "take mug", "north", "drop jam", "drop easter egg", "drop monolith", "drop mug", "north"]
        for c in commands:
            for l in c:
                yield(ord(l))
            yield(10)

        # We are now at checkpoint - code below exhausts the options, but I've added the requirements to the list above
        # before commiting
        #items = ["fuel cell", "jam", "shell", "easter egg", "space heater", "monolith", "coin", "mug"]
        #for numitems in range(8,0,-1):
        #    for testcomb in itertools.combinations(items,numitems):
        #        for item in items:
        #            for l in "drop " + item:
        #                yield(ord(l))
        #            yield(10)
        #        for item in testcomb:
        #            for l in "take " + item:
        #                yield(ord(l))
        #            yield(10)
        #        for l in "north":
        #            yield(ord(l))
        #        yield(10)
        #
        #while True:
        #    buffer = input("*INPUT*: ")
        #    for c in buffer:
        #        yield ord(c)
        #    yield(10)

    adv = intcode.CPU(ins, inputter, outputter)
    adv.run()

def main():
    ins = [int(x) for x in next(fileinput.input()).strip().split(',')]
    runAdventure(ins)
    
if __name__ == "__main__":
    main()
