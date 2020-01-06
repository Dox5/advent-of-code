#!/usr/bin/python

import fileinput
import collections
import functools

omap = {}

# Not a big difference with small input, but adding a cache makes
# the problem much faster on larger inputs using this method
@functools.lru_cache
def towardCOM(moon):
    return 0 if moon == 'COM' else towardCOM(omap[moon]) + 1

def route(moon):
    return [] if moon == 'COM' else route(omap[moon]) + [moon]

def orbitCount():
    return sum([towardCOM(moon) for moon in omap])

def santa():
    santa_route = route('SAN')
    you_route = route('YOU')
    uniq_parts = set(santa_route) ^ set(you_route)
    return len(uniq_parts)-2
    
def main():
    global omap
    orbits = [x.split(')') for x in (x.strip() for x in list(fileinput.input()))]
    omap = {y:x for x,y in orbits}

    print(orbitCount())
    print(santa())
    
if __name__ == "__main__":
    main()
