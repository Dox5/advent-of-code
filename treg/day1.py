#!/usr/bin/python3

import fileinput

def fuel(x):    
    return (x//3)-2

def calcFuel(fuelRequirement, descend=0):
    totalFuel = 0
    while(fuelRequirement := fuel(fuelRequirement)) > 0:
        totalFuel += fuelRequirement
        if not descend: break
    return totalFuel

def part1():
    fuel = [calcFuel(int(x)) for x in lines]
    return sum(fuel)

def part2():
    fuel = [calcFuel(int(x), 1) for x in lines]
    return sum(fuel)

if __name__ == "__main__":
    # Create a list of lines (strings) from the input file
    lines = list(fileinput.input())

    print(part1())
    print(part2())
