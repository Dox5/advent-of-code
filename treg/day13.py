#!/usr/bin/python

import fileinput
import intcode
import collections
import os

EMPTY = 0
WALL = 1
BLOCK = 2
PADDLE = 3
BALL = 4

def part1(ins):
    screen = {}
    def outputter():
        buf = []
        def o(x):
            nonlocal buf
            nonlocal screen
            buf.append(x)
            if len(buf) == 3:
                pos = complex(buf[0], -buf[1])
                screen[pos] = buf[2]
                buf = []
        return o

    cpu = intcode.CPU(ins,None,outputter())
    cpu.run()

    blocks = sum([1 for x in screen.values() if x == BLOCK])
    return blocks

def printscreen(screen, score):
    mx = int(max([x.real for x in screen.keys()]))
    my = int(max([abs(x.imag) for x in screen.keys()]))

    for y in range(my+1):
        for x in range(mx+1):
            item = screen[complex(x,-y)]
            if item == EMPTY:
                print(' ', end='')
            elif item == WALL:
                print('O', end='')
            elif item == BLOCK:
                print('.', end='')
            elif item == PADDLE:
                print('X', end='')
            else:
                print('*', end='')
        print()
    print("Score", score)
    print()

def part2(ins):
    screen = collections.defaultdict(lambda: EMPTY, {})
    score = 0
    def outputter():
        buf = []
        def o(x):
            nonlocal buf
            nonlocal screen
            nonlocal score
            buf.append(x)
            if len(buf) == 3:
                pos = complex(buf[0], -buf[1])
                if pos == complex(-1,0):
                    score = buf[2]
                else:
                    screen[pos] = buf[2]
                buf = []
        return o

    def input():
        nonlocal screen
        nonlocal score

        while True:
            printscreen(screen, score)
            paddlex = [x for x in screen.keys() if screen[x] == PADDLE][0]
            ballx = [x for x in screen.keys() if screen[x] == BALL][0]
            if ballx.real < paddlex.real:
                yield -1
            elif ballx.real > paddlex.real:
                yield 1
            else:
                yield 0

    ins[0] = 2 # Set free play
    cpu = intcode.CPU(ins,input,outputter())
    while not cpu.finished():
        cpu.run()

    return score
    
def main():    
    ins = [int(x) for x in next(fileinput.input()).strip().split(',')]
    print(part1(ins))
    print(part2(ins))
    
if __name__ == "__main__":
    main()
