#!/usr/bin/python
import fileinput
import networkx
import collections
import string
import itertools
import sys

def getNeighbours(md, pos):
    moves = [1+0j,-1+0j,0+1j,0-1j]
    neigh = [option+pos for option in moves]
    openneigh = [p for p in neigh if md[p] == '.']

    # Portals are two squares in either direction.
    portals = [[md[pos+x],md[pos+(2*x)]] for x in [1+0j,0+1j]]    
    portals = portals + [[md[pos+(2*x)],md[pos+x]] for x in [-1+0j,0-1j]]
    portals = [''.join(x) for x in portals]
    portals = [x for x in portals if x.isalpha() and x.isupper()]
    
    if len(portals) > 1:
        print("Mistakes made")
        raise

    if len(portals) == 0:
        # No portals
        portals = None
    else:        
        portals = portals[0]
    
    return(portals, zip(openneigh, itertools.repeat(pos)))
    
def buildMaze(ms, levels=1):
    # Dict first
    md = collections.defaultdict(lambda: ' ')
    for y, row in enumerate(ms):
        for x, val in enumerate(row):
            md[complex(x,y)] = val

    x = int(max([x.real for x in md.keys()]))
    y = int(max([x.imag for x in md.keys()]))

    # Look for '.' and wire them up. When doing neighbour searching remember    
    # positions of portals to wire up later
    maze = networkx.Graph()
    portals = collections.defaultdict(list)
    for mx in range(x):
        for my in range(y):
            pos = complex(mx,my)
            if md[pos] == '.':
                (portal,neighbours) = getNeighbours(md, pos)
                for n in neighbours:
                    for i in range(levels):
                        maze.add_edge((i,n[0]),(i,n[1]))

                if portal:
                    portals[portal].append(pos)

    for k,v in portals.items():
        if len(v) == 2:
            if levels == 1:
                maze.add_edge((0, v[0]), (0, v[1]))
            else:
                # If inputs different dimensions, this should be made cleaner :)
                if v[0].real == 2 or v[0].real == 120 or v[0].imag == 2 or v[0].imag == 118:
                    outer = v[0]
                    inner = v[1]
                else:
                    outer = v[1]
                    inner = v[0]
                for i in range(levels):
                    maze.add_edge((i, inner), (i+1, outer))
                    maze.add_edge((i+1, outer), (i, inner))
        else:
            if k not in ["AA", "ZZ"]:
                print(k,v)
                print("More mistakes made")
                raise Exception()

    start = (0, portals["AA"][0])
    finish = (0, portals["ZZ"][0])

    return (maze, start, finish)


def main():    
    mazestr = [line.rstrip("\n\r") for line in fileinput.input(sys.argv[1])]
    (maze, start, finish) = buildMaze(mazestr)
    s_to_f = networkx.shortest_path(maze, start, finish)
    print(len(s_to_f)-1)

    # Change me for different inputs :)
    if len(sys.argv) == 3:
        EXPLORATION_LIMIT = int(sys.argv[2])
    else:
        EXPLORATION_LIMIT = 26
    
    (maze, start, finish) = buildMaze(mazestr, EXPLORATION_LIMIT)
    s_to_f = networkx.shortest_path(maze, start, finish)
    print(len(s_to_f)-1)
    
if __name__ == "__main__":
    main()
