#!/usr/bin/python
import fileinput
import networkx
import string
import copy
import collections
import itertools

def getOpenPaths(mazemap, pos):
    neigh = [option+pos for option in [1+0j,-1+0j,0+1j,0-1j]]
    openneigh = [p for p in neigh if mazemap[p] in string.ascii_lowercase + '.' + '@']
    return zip(openneigh, itertools.repeat(pos))

def buildGraph(mazestr):
    keys = {}
    doors = {}
    start = None
    maze = networkx.Graph()

    mazemap = collections.defaultdict(lambda: '#')
    
    # Quick conversion into map first
    for y, row in enumerate(mazestr):
        for x, val in enumerate(row.strip()):
            mazemap[complex(x,-y)] = val

    # Now convert mazemap into various graphs
    for my in range(0,-y-1,-1):
        for mx in range(x+1):
            pos = complex(mx,my)
            val = mazemap[pos]

            # Construct appropriate info
            if val in string.ascii_lowercase:
                # This is a key. Navigable
                keys[val] = complex(mx,my)
                maze.add_edges_from(getOpenPaths(mazemap, pos))
            elif val in string.ascii_uppercase:
                # This is a door. Not navigable yet
                # Save off the maze change in doors hash. Not this only works if
                # no two doors are next to each other...
                doors[val.lower()] = networkx.Graph(getOpenPaths(mazemap, pos))
            elif val != '#':
                # Either a space or our entrance. Navigable
                maze.add_edges_from(getOpenPaths(mazemap, pos))

                # Remember our start position
                if val == '@':
                    start = pos

    return (maze, doors, keys, start)

def isVisible(maze, start, otherpos):
    try:
        return networkx.shortest_path(maze, start, otherpos)
    except:
        return None

# A nasty global to track the optimal path
bestpath = None
padding = ""

def explore(maze, doors, keys, pos, path):
    global bestpath
    global padding

    padding += " "
    
    # If the current best is better than our current, bail it's no good
    if bestpath and len(path) > len(bestpath):
        padding = padding[:-1]
        return
    
    # If there are no visible keys, then we've got them all. Path is now a contender!
    if len(keys) == 0:
        if not bestpath or len(bestpath) > len(path):
            print(padding, "NEW BEST:", len(path))
            bestpath = copy.deepcopy(path)
        padding = padding[:-1]            
        return

    # Checks passed so far, lets look for keys!
    visiblekeys = [k for k in keys if isVisible(maze, pos, keys[k])]    
    for k in visiblekeys:
        print(padding, "Trying key", k)
        # We've chosen to go to k.
        kpath = isVisible(maze, pos, keys[k])

        # We don't want to mutate the state we currently have, so copy things into the recursive call
        newmaze = copy.deepcopy(maze)
        newmaze.add_edges_from(doors[k].edges())
        newkeys = copy.deepcopy(keys)
        newkeys.pop(k)
        newpath = copy.deepcopy(path) + kpath[1:]
        explore(newmaze, doors, newkeys, keys[k], newpath)            

    padding = padding[:-1]
                      
        
def solveMaze(maze, doors, keys, start):
    path = []
    explore(maze, doors, keys, start, path)

def main():
    mazestr = fileinput.input()
    (maze, doors, keys, start) = buildGraph(mazestr)

    # OK, now we know lots about the maze, we need a way of finding the next key
    # and then adding the door navigability in, rinse and repeat.

    # We can probably ask the graph for shortest path to all keys from our current
    # position. How do we choose which is optimal though, since it's not always
    # the closest. We could just recurse, copying the mazemap and picking different
    # options tracking how far we've walked. If we've walked longer than curernt
    # max, abandon that branch.
    solveMaze(maze,doors,keys,start)

    global bestpath
    print(len(bestpath))
    
if __name__ == "__main__":
    main()
