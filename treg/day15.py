#!/usr/bin/python
import fileinput
import intcode
import networkx

steps = {1: 0+1j, 2: 0-1j, 3:-1+0j, 4:1+0j}
pathrev = {1: 2, 2: 1, 3: 4, 4: 3}
oxygen = None

def findOxygen(ins):
    # Try building a graph of our progress
    G = networkx.Graph()
    G.add_node(0+0j)

    # Position
    pos = 0+0j
    options = {0+0j: [1,2,3,4]}
    path = []
    reverse = False
    
    # Moving around is hard!
    def inputter():
        nonlocal pos
        nonlocal reverse
        nonlocal path
        lastpos = pos
        while True:
            # We find ourselves at pos, with some options in options. Pick the next and
            # execute it if there are any. If no options remain, choose the direction to
            # send us back one
            if len(options[pos]) == 0:
                reverse = True
                yield pathrev[path[-1]] # This will throw when we've exhausted everything
            else:
                go = options[pos].pop(-1)
                path.append(go)
                yield go
            

    def outputter(response):
        nonlocal pos
        nonlocal reverse
        global oxygen

        if response == 0:
            # Hit a wall. Remove from path, don't update position
            path.pop(-1)
        else:
            if not reverse:
                # Found a spot we could move into. Could be a place we've encountered
                # before if there are loops
                newpos = pos + steps[path[-1]]

                # Add a link in the graph
                G.add_edge(pos, newpos)

                # Update pos
                pos = newpos

                if pos not in options:
                    options[pos] = [x for x in range(1,5) if x != pathrev[path[-1]]]

                # Remeber if this is oxygen
                if response == 2:
                    oxygen = pos
            else:
                # Unwind
                newpos = pos + steps[pathrev[path[-1]]]
                pos = newpos
                reverse = False
                path.pop(-1)
            
    cpu = intcode.CPU(ins, inputter, outputter)
    try:
        cpu.run()
    except Exception as e:
        pass

    return G


def main():
    ins = [int(x) for x in next(fileinput.input()).strip().split(',')]
    layout = findOxygen(ins)
    print(networkx.shortest_path_length(layout, 0+0j, oxygen))

    paths = networkx.algorithms.single_source_shortest_path(layout, oxygen)
    furthest = max(paths, key=lambda k: len(paths[k]))
    print(len(paths[furthest])-1)

    
if __name__ == "__main__":
    main()
