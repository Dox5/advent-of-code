import typing
import doctest
import itertools
from collections import deque
import cProfile

import enum

example_1 = [
    "...........",
    ".....###.#.",
    ".###.##..#.",
    "..#.#...#..",
    "....#.#....",
    ".##..S####.",
    ".##..#...#.",
    ".......##..",
    ".##.#.####.",
    ".##..##.##.",
    "...........",
]

class Loc(enum.IntEnum):
    Rock  = 0
    Plot  = 1
    Start = 2

    @classmethod
    def from_c(cls, c: str) -> "Loc":
        if c == "S":
            return Loc.Start
        elif c == "#":
            return Loc.Rock
        elif c == ".":
            return Loc.Plot
        else:
            raise ValueError(c)


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

    def __mod__(self, other):
        """
        >>> Point(x=8, y=11) % Point(x=3, y=11)
        Point(x=2, y=0)
        """
        return Point(x=self.x % other.x, y=self.y % other.y)

class Grid:
    def __init__(self, lines: str, infinite=False):
        self.height = len(lines)
        self.width = len(lines[0])
        self.infinite = infinite

        self.cells = [ Loc.from_c(c) for c in itertools.chain.from_iterable(lines) ]

        self.bounds = (range(self.width), range(self.height))

    def __getitem__(self, p: Point):
        """
        >>> Grid(example_1)[Point(x=3, y=2)]
        '#'
        """
        if self.infinite:
            p = p % Point(self.width, self.height)

        return self.cells[p.x + p.y * self.width]

    def __setitem__(self, p: Point, v: str):
        if self.infinite:
            p = p % Point(self.width, self.height)

        self.cells[p.x + p.y * self.width] = v


    def __contains__(self, p: Point) ->  bool:
        if self.infinite:
            return True

        x_bound, y_bound = self.bounds
        return p.x in x_bound and p.y in y_bound

    def find_start(self) -> Point:
        """
        >>> Grid(example_1).find_start()
        Point(x=5, y=5)
        """
        idx = self.cells.index(Loc.Start)
        y = int(idx/self.width)
        x = idx % self.width
        return Point(x=x, y=y)


def dump_grid(g: Grid, overlay: dict[Point, str]):
    for y in range(g.height):
        for x in range(g.width):
            p = Point(x=x, y=y)
            try:
                c = overlay[p]
            except KeyError:
                c = g[p]

            print(c, end="")
        print()

def reachable_plots(g: Grid, desired_steps: int) -> int:
    """
    >>> reachable_plots(Grid(example_1), 6)
    16
    >>> reachable_plots(Grid(example_1, infinite=True), 6)
    16
    >>> reachable_plots(Grid(example_1, infinite=True), 10)
    50
    >>> reachable_plots(Grid(example_1, infinite=True), 50)
    1594
    >>> reachable_plots(Grid(example_1, infinite=True), 100)
    6536
    >>> reachable_plots(Grid(example_1, infinite=True), 500)
    167004
    >>> reachable_plots(Grid(example_1, infinite=True), 1000)
    668697
    >>> reachable_plots(Grid(example_1, infinite=True), 5000)
    16733044
    """
    seen = {}

    # BFS so we explore the shortest paths first. All steps are equally weighted
    # so a simple queue will work here as every produced path must be longer
    # or the same length as those already in the queue
    to_explore : deque[tuple[Point, int]] = deque()

    watermark = 0

    start = g.find_start()
    to_explore.append((start, 0))
    seen[start] = 0

    while  True:
        try:
            p, steps = to_explore.popleft()
        except IndexError:
            # Done
            break

        #if len(to_explore) > watermark:
        #    print("High watermark for to_explore", len(to_explore), "seen", len(seen))
        #    watermark = len(to_explore)

        if steps >= desired_steps:
            # Cull this walk, no further steps make sense. Equal to because
            # we've already handled this point (which is exactly desired_steps
            # away) so we don't need to handle any others
            continue

        for offset in [Point(x=1, y=0), Point(x=-1, y=0), Point(x=0, y=1), Point(x=0, y=-1)]:
            neigh = p + offset
            if all([neigh in g, not neigh in seen, g[neigh] != Loc.Rock]):
                # This is the first time we've seen this location, remember
                # how many steps it took to get here. This should be the
                # shortest path

                to_explore.append((neigh, steps+1))
                seen[neigh] = steps+1

    #dump_grid(g, {p: "O" for p, v in seen.items()})


    is_even = lambda i: i % 2 == 0 

    want_evenness = is_even(desired_steps)

    count = 0

    for v in seen.values():
        if (v % 2 == 0) == want_evenness and v <= desired_steps:
            count += 1

    return count


if __name__ == "__main__":
    doctest.testmod(verbose=True)
    g = Grid(example_1, infinite=True)
    cProfile.run("reachable_plots(g, 1000)")

    with open("input.txt") as fh:
        lines = [l.strip() for l in fh]
        g = Grid(lines)
        #print("Reachable in exactly 64 steps", reachable_plots(g, 64))
        infinite_g = Grid(lines, infinite=True)
        #print("Reachable in exactly 26501365 steps", reachable_plots(infinite_g, 26501365))
