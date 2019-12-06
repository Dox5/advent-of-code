#!/usr/bin/python

import fileinput
import collections

def orbitCount(omap, start, depth=-1):
    depth += 1
    subs = 0
    for d in omap[start]:
        subs += orbitCount(omap, d, depth)

    return depth+subs

def host(omap, id):
    return [x for x in omap if id in omap[x]][0]

def route(omap, id):
    s = [host(omap, id)]
    while s[-1] != 'COM':
        s.append(host(omap, s[-1]))
    return s

def toSanta(omap, start='YOU', depth=0):
    youhost = route(omap, 'YOU')
    sanhost = route(omap, 'SAN')
           
    onlysan = set(sanhost) - set(youhost)
    onlyyou = set(youhost) - set(sanhost)
    
    return len(onlysan)+len(onlyyou)

def main():
    l = list(fileinput.input())
    orbits = [(x[0],x[1][:-1]) for x in (x.split(')') for x in l)]
    omap = collections.defaultdict(list)
    for (x,y) in orbits:
        omap[x].append(y)
        if not y in omap:
            omap[y] = []

    print(orbitCount(omap,'COM'))
    print(toSanta(omap))
    
if __name__ == "__main__":
    main()
