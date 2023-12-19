import itertools
import typing
import doctest
import collections

class Point(typing.NamedTuple):
    x: int
    y: int

    def __add__(self, other):
        """
        >>> Point(x=5, y=10) + Point(x=1, y=2)
        Point(x=6, y=12)
        """
        return Point(x=self.x + other.x, y=self.y + other.y)

    def __sub__(self, other):
        """
        >>> Point(x=5, y=10) - Point(x=1, y=2)
        Point(x=4, y=8)
        """
        return Point(x=self.x - other.x, y=self.y - other.y)

example_1 = [
    "O....#....",
    "O.OO#....#",
    ".....##...",
    "OO.#O....O",
    ".O.....O#.",
    "O.#..O.#.#",
    "..O..#O..O",
    ".......O..",
    "#....###..",
    "#OO..#....",
]

class Grid:
    def __init__(self, lines: str):
        self.height = len(lines)
        self.width = len(lines[0])

        self.cells = [ c for c in itertools.chain.from_iterable(lines) ]

    def __getitem__(self, p: Point):
        """
        >>> Grid(example_1)[Point(x=4, y=1)]
        '#'
        """
        return self.cells[p.x + p.y * self.width]

    def __setitem__(self, p: Point, v: str):
        self.cells[p.x + p.y * self.width] = v

    # This is a stupid function
    def tuplise(self):
        return tuple(self.cells)

    def tilt_north_south(self, north=True):
        y_range = range(self.height) if north else range(self.height-1, -1, -1)

        # Handle each column separately
        for x in range(self.width):
            # Track the next available position that can be filled
            free = collections.deque()

            # Walk the appropriate way though the Y values to find the gaps
            for y in y_range:
                c = self[Point(x, y)]

                if c == ".":
                    # Found a new free spot that can be used by a rock
                    free.append(y)
                elif c == "#":
                    # Everything that was free cannot now be used as it's
                    # blocked
                    free.clear()
                elif c == "O":
                    # A rock! Can we move it up?
                    try:
                        new_pos = free.popleft()
                        self[Point(x=x, y=new_pos)] = "O"
                        self[Point(x=x, y=y)] = "."
                        # This space is now also free
                        free.append(y)

                    except IndexError:
                        # Can't move this rock as there was no spaces available
                        pass

    def tilt_east_west(self, east=True):
        x_range = range(self.width) if not east else range(self.width -1, -1, -1)

        # Handle each row separately
        for y in range(self.height):
            # Track the next available position that can be filled
            free = collections.deque()

            # Walk the appropriate way though the Y values to find the gaps
            for x in x_range:
                c = self[Point(x, y)]

                if c == ".":
                    # Found a new free spot that can be used by a rock
                    free.append(x)
                elif c == "#":
                    # Everything that was free cannot now be used as it's
                    # blocked
                    free.clear()
                elif c == "O":
                    # A rock! Can we move it up?
                    try:
                        new_pos = free.popleft()
                        self[Point(x=new_pos, y=y)] = "O"
                        self[Point(x=x, y=y)] = "."
                        # This space is now also free
                        free.append(x)

                    except IndexError:
                        # Can't move this rock as there was no spaces available
                        pass

    def north_load(self):
        total = 0

        for x in range(self.width):
            for y in range(self.height):
                if self[Point(x=x, y=y)] == "O":
                    total += self.height - y

        return total

    def spin_cycle(self):
        self.tilt_north_south(north=True)
        self.tilt_east_west(east=False)
        self.tilt_north_south(north=False)
        self.tilt_east_west(east=True)

    def __repr__(self):
        lines = []
        for y in range(self.height):
            start = y * self.width
            end = start + self.width
            lines.append("".join(self.cells[start:end]))
        return "\n".join(lines)

def test_spin():
    g = Grid(example_1)
    g.spin_cycle()

def part_a(lines):
    """
    >>> part_a(example_1)
    136
    """
    g = Grid(lines)
    g.tilt_north_south(north=True)

    return g.north_load()

def part_b(lines):
    g = Grid(lines)
    path = [g.tuplise()]
    seen = set([g.tuplise()])
    
    while True:
        g.spin_cycle()
        t = g.tuplise()
        if t in seen:
            # Found the loop!
            break
        path.append(t)
        seen.add(t)

    # We know there is a loop now, find where it starts in path
    for i, p in enumerate(path):
        if p == g.tuplise():
            break

    target = 1000000000

    spins_to_align = i
    loop_length = len(seen) - i

    required_spins = spins_to_align + ((target - spins_to_align) % loop_length) 

    g2 = Grid(lines)
    for _ in range(required_spins):
        g2.spin_cycle()

    return g2.north_load()




if __name__ == "__main__":
    doctest.testmod()

    test_spin()

    assert part_b(example_1) == 64

    with open("input.txt") as fh:
        lines = [l.strip() for l in fh]
        print("Total load on north", part_a(lines))
        print("Total load on north (spin cycle)", part_b(lines))
