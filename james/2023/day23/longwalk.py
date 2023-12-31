import enum
import networkx
from tqdm import tqdm
import itertools

from matplotlib import pyplot as plt
from matplotlib.pyplot import figure

class Tile(enum.Enum):
    Path       = "."
    Forest     = "#"
    SlopeNorth = "^"
    SlopeEast  = ">"
    SlopeSouth = "v"
    SlopeWest  = "<"

class Grid:
    def __init__(self, lines: list[str]):
        self.height = len(lines)
        self.width = len(lines[0])

        self._cells = [ Tile(t) for l in lines for t in l ]

        assert self.height * self.width == len(self._cells)

    def __getitem__(self, pos):
        x, y = pos
        return self._cells[x + y * self.width]

    def neighbours_of(self, pos, slippery_slopes=True):
        neighbours = []
        x, y = pos

        # Workout where we can go based on the tile we're stood on
        t = self[pos]
        allowed_steps = ((1, 0), (-1, 0), (0, 1), (0, -1))

        if slippery_slopes:
            if t == Tile.SlopeNorth:
                allowed_steps = ((0, -1),)
            elif t == Tile.SlopeEast:
                allowed_steps = ((1, 0),)
            elif t == Tile.SlopeSouth:
                allowed_steps = ((0, 1),)
            elif t == Tile.SlopeWest:
                allowed_steps = ((-1, 0),)

        # Determine which directions are inside the grid and also not a forest
        for ox, oy in allowed_steps:
            nx = x + ox
            ny = y + oy
            
            if nx in range(0, self.width) and ny in range(0, self.height):
                t = self[(nx, ny)]

                if t == Tile.Forest:
                    # Can't go here
                    continue

                # Can go this way
                neighbours.append((nx, ny))

        return neighbours
                




example_grid = Grid([
    "#.#####################",
    "#.......#########...###",
    "#######.#########.#.###",
    "###.....#.>.>.###.#.###",
    "###v#####.#v#.###.#.###",
    "###.>...#.#.#.....#...#",
    "###v###.#.#.#########.#",
    "###...#.#.#.......#...#",
    "#####.#.#.#######.#.###",
    "#.....#.#.#.......#...#",
    "#.#####.#.#.#########v#",
    "#.#...#...#...###...>.#",
    "#.#.#v#######v###.###v#",
    "#...#.>.#...>.>.#.###.#",
    "#####v#.#.###v#.#.###.#",
    "#.....#...#...#.#.#...#",
    "#.#########.###.#.#.###",
    "#...###...#...#...#.###",
    "###.###.#.###v#####v###",
    "#...#...#.#.>.>.#.>.###",
    "#.###.###.#.###.#.#v###",
    "#.....###...###...#...#",
    "#####################.#",
])

def solve(grid: Grid, slippery_slopes=True):

    # Find start and end
    for x in range(0, grid.width):
        if grid[(x, 0)] == Tile.Path:
            start = (x, 0)

        if grid[(x, grid.height - 1)] == Tile.Path:
            end = (x, grid.height - 1)

    g = networkx.DiGraph()

    # Walk all points and build up the graph
    for y in range(grid.height):
        for x in range(grid.width):
            if grid[(x, y)] != Tile.Forest:
                neighbours = grid.neighbours_of((x, y), slippery_slopes)

                # Add edges that can be taken
                for n in neighbours:
                    # All paths are length 1 at this point
                    g.add_edge((x, y), n, length=1)


    # Simplify, any arrangements like the following
    # A - ab -> B - bc -> C
    # can be turned into
    # A <- ab + bc -> C
    made_change = True
    while made_change:
        made_change = False

        # Find and remove isolated nodes (because they've been replaced
        isolated = networkx.isolates(g)
        g.remove_nodes_from(list(isolated))

        # Walk every node in the graph
        for n in g:
            pred = list(g.predecessors(n))
            succ = list(g.successors(n))
            if len(pred) == 2 and sorted(pred) == sorted(succ):
                # This node can be eliminated
                made_change = True

                # Pick a node for either side, it doesn't really matter which is
                # which
                a, b = pred

                # handle a -> n -> b first
                g.add_edge(a, b, length=g.edges[a, n]["length"] + g.edges[n, b]["length"])
                # Now remove the old edges
                g.remove_edge(a, n)
                g.remove_edge(n, b)

                # handle b -> n -> a now
                g.add_edge(b, a, length=g.edges[b, n]["length"] + g.edges[n, a]["length"])
                g.remove_edge(b, n)
                g.remove_edge(n, a)





    #figure(figsize=(14,14), dpi=120)
    #pos = networkx.spectral_layout(g)
    #networkx.draw_networkx(g, pos, with_labels=True)
    #networkx.draw_networkx_edge_labels(g, pos, edge_labels={e: g.edges[e]["length"] for e in g.edges})
    #plt.savefig("graph.png")

    # Be dumb
    longest = 0
    for p in tqdm(networkx.all_simple_edge_paths(g, start, end)):
        cost = sum(g.edges[f, t]["length"] for f, t in p)
        if cost > longest:
            print("best", cost)
            longest = cost

    return longest







if __name__ == "__main__":
    print("(example) longest hike:", solve(example_grid))
    print("(example) longest hike (non-slippery):", solve(example_grid, slippery_slopes=False))

    with open("input.txt") as fh:
        g = Grid([l.strip() for l in fh])


    print("Longest hike:", solve(g))
    print("Longest hike (non-slippery):", solve(g, slippery_slopes=False))
