#!/usr/bin/python
import math
import fileinput
import networkx
import string
import copy
import collections
import itertools

def getOpenPaths(mazemap, pos):
    neigh = [option+pos for option in [1+0j,-1+0j,0+1j,0-1j]]
    openneigh = [p for p in neigh if mazemap[p] != '#']
    return zip(openneigh, itertools.repeat(pos))

def buildGraph(mazestr):
    keys = {}
    doors = {}
    start = []
    maze = networkx.Graph()

    mazemap = collections.defaultdict(lambda: '#')
    
    # Quick conversion into map first
    for y, row in enumerate(mazestr):
        for x, val in enumerate(row.strip()):
            mazemap[complex(x,-y)] = val

    # Now convert mazemap into graph
    for my in range(0,-y-1,-1):
        for mx in range(x+1):
            pos = complex(mx,my)
            val = mazemap[pos]

            # Construct appropriate info
            if val in string.ascii_lowercase:
                keys[val] = complex(mx,my)
                maze.add_edges_from(getOpenPaths(mazemap, pos))
            elif val in string.ascii_uppercase:
                doors[val.lower()] = complex(mx,my)
                maze.add_edges_from(getOpenPaths(mazemap, pos))                
            elif val != '#':
                # Either a space or our entrance. Navigable
                maze.add_edges_from(getOpenPaths(mazemap, pos))

                # Remember our start position
                if val == '@':
                    start.append(pos)

    return (maze, doors, keys, start)

def generateRoutes(maze,door,keys,start):
    options = {}
    for idx, route in enumerate(itertools.combinations(list(keys.values()) + start, 2)):
        print("Precalculating Route", idx, route)
        
        # What are all the paths between these nodes
        paths = networkx.all_simple_paths(maze, route[0], route[1])
        
        # What constraints exist on each of these paths        
        for p in paths:            
            keysneeded = set()
            for d in door.keys():
                if door[d] in p:
                    keysneeded.add(d)

            # There might be multiple viable routes that use the same key
            # Always select the smallest!
            if (route[0],route[1]) not in options:
                options[(route[0],route[1])] = [(keysneeded,len(p)-1)]
                options[(route[1],route[0])] = [(keysneeded,len(p)-1)]
            else:
                for x, v in enumerate(options[(route[0],route[1])]):
                    if v[0] == keysneeded and v[1] > len(p)-1:
                        options[(route[0],route[1])][x] = (keysneeded,len(p)-1)
                        options[(route[1],route[0])][x] = (keysneeded,len(p)-1)                        
                        break
                    
    return options

# Memory of future paths
infront = {}
def distanceAhead(options, keys, pos, mykeys):
    # Check to see if the state we are in has already been observed
    stringkeys = str(sorted(list(mykeys)))

    # Super arbitrary sort is super arbitrary
    stringpos = str(sorted(pos, key=lambda x: x.real + (x.imag*1000)))
    if (stringpos, stringkeys) in infront:
        return infront[stringpos,stringkeys]

    # Which keys are left
    keyToGo = set(keys.keys()) - mykeys
    
    # If there are no keys remaining, there is no distance ahead!
    if len(keyToGo) == 0:
        infront[(stringpos, stringkeys)] = 0
        return 0
    
    # We've got keys to explore and multiple places to be. Grab one and try it,
    # record all the distances we receive. Memorise the best one and return it
    distances = []

    # Erk we've got to choose which robot to move
    for ridx, robo in enumerate(pos):        
        for k in keyToGo:
            try:
                paths = options[(robo,keys[k])]
            except:
                paths = []
                
            validpaths = [length for kn, length in paths if kn.issubset(mykeys)]
            if validpaths:
                nk = copy.deepcopy(mykeys)
                nk.add(k)
                np = copy.deepcopy(pos)
                np[ridx] = keys[k]
                distances.append(distanceAhead(options,keys,np,nk) + min(validpaths))
            else:
                distances.append(math.inf)

    bestDistanceAhead = min(distances)
    infront[(stringpos,stringkeys)] = bestDistanceAhead
    return bestDistanceAhead
      
def main():
    mazestr = fileinput.input()

    # Build complete graph
    (maze, door, keys, start) = buildGraph(mazestr)
    
    # We could figure out what all shortest paths are between all keys (+ start positions)
    # We could know what constraints there are on choosing any given path (doors X, Y, Z)
    # Then the search becomes "select viable move", try it.

    # We could also view this as looking ahead, rather than finding the best absolute path. This
    #  allows us to remember what happens ahead of us in a path if we find ourselves back in the same
    #  spot - the path ahead is the same length if we start a,b,c as it is b,a,c - if we're at c
    #  (and any other robots are at their starts) the future moves are going to be the same

    # Multi robot support comes from knowing each robot can't reach each other, so we can use the
    #  same cache technique of looking forward but iterate over multiple possible robots moving
    options = generateRoutes(maze,door,keys,start)
    bda = distanceAhead(options,keys,start,set())
    print(bda)
        
            
if __name__ == "__main__":
    main()
