#!/usr/bin/python

import math
import copy
import fileinput
import re
import itertools

def cmp(a, b):
    if a < b:
        return 1
    elif a == b:
        return 0
    else:
        return -1

class Moon:
    def __init__(self, x,y,z,xd,yd,zd):
        self.pos = [x,y,z]
        self.vel = [xd,yd,zd]

    def __repr__(self):
        return self.__str__()

    def __str__(self):
        return str(self.pos) + str(self.vel)

    def velUpdate(self, other):
        delta = [cmp(self.pos[i], other.pos[i]) for i in range(3)]
        self.vel = [x+y for x,y in zip(self.vel,delta)]
        other.vel = [x-y for x,y in zip(other.vel,delta)]

    def moveMoon(self):
        self.pos = [x+y for x,y in zip(self.pos, self.vel)]

    def potential(self):
        return sum([abs(x) for x in self.pos])

    def kinetic(self):
        return sum([abs(x) for x in self.vel])

    def totalEnergy(self):
        return self.potential() * self.kinetic()

def part1(moons):
    while True:
        for (m1,m2) in itertools.combinations(moons,2):
            m1.velUpdate(m2)
        for m in moons:
            m.moveMoon()

        yield moons

def lcm(a, b):
        return abs(a*b) // math.gcd(a, b)

def main():
    input = fileinput.input()
    moonpos = [re.findall(r'-?[0-9]+', x) for x in input if x.strip()]
    moonpos = [[int(x),int(y),int(z)] for x,y,z in moonpos]
    moonvel = [[0,0,0] for _ in moonpos]
    moons = [Moon(*p,*v) for p,v in zip(moonpos,moonvel)]

    # Part 1 cast as generator to make part 2 easier
    moonGen = part1(list(moons))
    for i in range(1000):
        moons = next(moonGen)
    print(sum([m.totalEnergy() for m in moons]))

    # Part 2
    # Can't sep moons as the others would have differnet values
    # but the dimensions move independently... That allows us to
    # get least common multiple perhaps.
    # Very kludge, may tidy later
    moons = [Moon(*p,*v) for p,v in zip(moonpos,moonvel)]
    moonGen = part1(list(moons))

    cycles = [0] * 3
    dims = [{str([[m.pos[x],m.vel[x]] for m in moons]):0} for x in range(3)]
    for i in itertools.count(1):
        newMoons = next(moonGen)
        newDims = [str([[m.pos[x],m.vel[x]] for m in newMoons]) for x in range(3)]
        for d in range(3):
            if cycles[d] == 0:
                if newDims[d] in dims[d]:
                    cycles[d] = (dims[d][newDims[d]], i)
                else:
                    dims[d][newDims[d]] = i

        if all(cycles):
            break

    # This will break if cycle doesn't start at 0 time I think... Could probably
    # fix if we LCM the cycle start offsets and then figure out from there.
    print(cycles)
    print(lcm(lcm(cycles[0][1],cycles[1][1]),cycles[2][1]))

if __name__ == "__main__":
    main()
