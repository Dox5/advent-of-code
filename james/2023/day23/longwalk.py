import enum

from pprint import pprint

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

    # Keep track of things we've seen on this path (start of segments)
    seen = set()


    seg_cache = {}

    def expand_seg(seg):
        if len(seg) == 1:
            # Can try and use the cache for this
            try:
                return seg_cache[seg[0]]
            except KeyError:
                pass

        in_seg = set(seg)

        while True:
            neigh = g.neighbours_of(seg[-1], slippery_slopes)

            # Remove any nodes we have already seen or are part of this segment
            nxt = list((set(neigh) - seen) - in_seg)

            if len(nxt) == 1:
                # No decision to make
                in_seg.add(nxt[0])
                seg.append(nxt[0])
            else:
                # Either at the end or need to make a decision
                break

        # Cache this for the future 
        print(len(seg_cache))
        seg_cache[seg[0]] = seg
        return seg

    # Traversal stack, populated with our start element
    start_seg = expand_seg([start])
    path = [(start_seg, g.neighbours_of(start_seg[-1], slippery_slopes))]

    longest_path = 0

    while path != []:
        # This is the element we are currently working on
        seg, _ = path[-1] 


        if path[-1][1] != []:
            # More to explore still
            # Add this node to the seen set so we don't revisit it
            for p in seg:
                seen.add(p)

            nxt = path[-1][1].pop()

            if nxt not in seen:
                # Setup for the next exploration
                nxt_seg = expand_seg([nxt])
                next_neigh = set(g.neighbours_of(nxt_seg[-1], slippery_slopes))
                next_neigh -= seen
                path.append((nxt_seg, list(next_neigh)))

        else:
            if seg[-1] == end:
                # Found the end!
                this_path_len = sum(len(p[0]) for p in path)
                longest_path = max(longest_path, this_path_len)

            # Finished exploring here, go back up the stack
            # Need to remove this element from our seen set because we can go
            # back to it
            for p in seg:
                # It's possible to get here without adding these - due to going
                # to a node and never decending from it
                seen.discard(p)

            # Setup to go to the previous element
            path.pop()

    # -1 because the start point doesn't count as a step (fence post problem)
    return longest_path - 1


if __name__ == "__main__":
    print("(example) longest hike:", solve(example_grid))
    print("(example) longest hike (non-slippery):", solve(example_grid, slippery_slopes=False))

    with open("input.txt") as fh:
        g = Grid([l.strip() for l in fh])


    print("Longest hike:", solve(g))
    print("Longest hike (non-slippery):", solve(g, slippery_slopes=False))
