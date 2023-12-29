import enum

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

def solve(g: Grid, slippery_slopes=True):
    # Find start and end
    for x in range(0, g.width):
        if g[(x, 0)] == Tile.Path:
            start = (x, 0)

        if g[(x, g.height - 1)] == Tile.Path:
            end = (x, g.height - 1)

    # Traversal stack, populated with our start element
    path = [(start, g.neighbours_of(start, slippery_slopes))]

    # Remember the longest path we've seen to this pos
    cache = dict()

    # Keep track of things we've seen on this path
    seen = set([start])

    longest_path = 0

    while path != []:
        # This is the element we are currently working on
        pos, _ = path[-1] 

        #longest_to_pos = cache.get(pos, 0)
        #if len(path) >= longest_to_pos:
        #    # This is a better path so worth exploring, remember for next time
        #    print("Found longer path to", pos)
        #    cache[pos] = len(path)
        #else:
        #    # We previously found a longer path to this position, so this path
        #    # cannot be better so it can be culled
        #    seen.discard(pos)
        #    path.pop()
        #    continue

        if path[-1][1] != []:
            # More to explore still
            # Add this node to the seen set so we don't revisit it

            nxt = path[-1][1].pop()

            # We're putting this into the path so we add it to seen
            seen.add(nxt)

            # Setup for the next exploration
            neigh = set(g.neighbours_of(nxt, slippery_slopes))
            neigh -= seen
            neigh = list(neigh)
            path.append((nxt, neigh))

        else:
            if pos == end:
                # Found a route to the end
                longest_path = max(longest_path, len(path))

            # Can head back up now

            # Finished exploring here, go back up the stack
            # Need to remove this element from our seen set because we can go
            # back to it
            seen.remove(pos)
            # Setup to go to the previous element
            path.pop()

    # -1 because the start point doesn't count as a step (fence post problem)
    return longest_path - 1


if __name__ == "__main__":
    print("(example) longest hike:", solve(example_grid))
    print("(example) longest hike (non-slippery):", solve(example_grid, slippery_slopes=False))

    #with open("input.txt") as fh:
    #    g = Grid([l.strip() for l in fh])


    #print("Longest hike:", solve(g))
    #print("Longest hike (non-slippery):", solve(g, slippery_slopes=False))
