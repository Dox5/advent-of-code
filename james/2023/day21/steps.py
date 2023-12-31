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

    def __getitem__(self, p: tuple[int, int]):
        """
        >>> Grid(example_1)[(3, 2)]
        '#'
        """
        x, y = p
        if self.infinite:
            x = x % self.width
            y = y % self.height

        return self.cells[x + y * self.width]

    def __setitem__(self, p: tuple[int, int], v: Loc):
        x, y = p
        if self.infinite:
            x = x % self.width
            y = y % self.height

        self.cells[x + y * self.width] = v


    def __contains__(self, p: tuple[int, int]) ->  bool:
        if self.infinite:
            return True

        x, y = p
        x_bound, y_bound = self.bounds
        return x in x_bound and y in y_bound

    def find_start(self) -> tuple[int, int]:
        """
        >>> Grid(example_1).find_start()
        (5, 5)
        """
        idx = self.cells.index(Loc.Start)
        y = int(idx/self.width)
        x = idx % self.width
        return x, y


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


def steps_to_plots(g: Grid, start_points: list[tuple[int, int]], start_value=0, limit=-1) -> list[tuple[int, int], int]:

    # BFS so we explore the shortest paths first. All steps are equally weighted
    # so a simple queue will work here as every produced path must be longer
    # or the same length as those already in the queue
    to_explore : deque[tuple[tuple[int, int], int]] = deque([(p, start_value) for p in start_points])
    seen = {
        p: start_value for p in start_points
    }

    while  True:
        try:
            (x, y), steps = to_explore.popleft()
        except IndexError:
            # Done
            break

        if limit != -1 and steps > limit:
            continue

        for off_x, off_y in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            neigh = (x + off_x, y + off_y)
            if neigh in g and not neigh in seen and g[neigh] != Loc.Rock:
                # This is the first time we've seen this location, remember
                # how many steps it took to get here. This should be the
                # shortest path

                to_explore.append((neigh, steps+1))
                seen[neigh] = steps+1

    return seen.items()


def reachable_plots(g: Grid, desired_steps: int) -> int:
    """
    >>> reachable_plots(Grid(example_1), 6)
    16
    """

    steps = steps_to_plots(g, [g.find_start()], limit=desired_steps)

    want_evenness = desired_steps % 2 == 0

    count = 0

    for (_, v) in steps:
        if (v % 2 == 0) == want_evenness and v <= desired_steps:
            count += 1

    return count

def combine(edge_steps, edge_range):
    x_range, y_range = edge_range
    longest = 0
    even_steps = 0
    odd_steps = 0
    # Assumption: even_steps is the same no matter the entry point
    for x in x_range:
        for y in y_range:
            longest = max(longest, edge_steps[(x, y)]["longest"])
            even_steps = edge_steps["even_steps"]
            odd_steps = edge_steps["odd_steps"]


    return {
            "longest": longest,
            "even_spaces": even_spaces,
            "odd_steps": odd_steps,
    }
def make_next_level(level):

    prev = None
    for v in level:
        if prev is not None:
            yield v - prev
        prev = v

def do_dumb(level: list[int], forwards=True) -> int:
    """
    >>> do_dumb([0, 3, 6, 9, 12, 15])
    18
    >>> do_dumb([0, 3, 6, 9, 12, 15], forwards=False)
    -3

    >>> do_dumb([1, 3, 6, 10, 15, 21])
    28
    >>> do_dumb([1, 3, 6, 10, 15, 21], forwards=False)
    0

    >>> do_dumb([10, 13, 16, 21, 30, 45])
    68
    >>> do_dumb([10, 13, 16, 21, 30, 45], forwards=False)
    5
    """
    levels = [list(make_next_level(level))]

    while True:
        if all(l == 0 for l in levels[-1]):
            break;
        levels.append(list(make_next_level(levels[-1])))

    if forwards:
        return level[-1] + sum(l[-1] for l in levels)
    else:

        firsts = [level[0]] + [l[0] for l in levels]

        new_firsts = []

        prev = 0
        for f in reversed(firsts):
            # Walk backwards
            x = f - prev
            new_firsts.append(x)
            prev = x

        return new_firsts[-1]

def reachable_plots_infinite(g: Grid, target_steps):
    """
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

    # Generate sequence while we can still be bothered to wait for it
    n = 0
    seq = [reachable_plots(g, n) for n in [65, 196, 327, 458]]
    n = 3

    while n != 202300:
        n += 1
        seq.append(do_dumb(seq))

        # Don't let this grow too big for speed purposes
        if len(seq) > 200:
            seq = seq[50:]

    print(seq[-1])


        
        








if __name__ == "__main__":
    #doctest.testmod(verbose=True)
    #g = Grid(example_1, infinite=True)
    #cProfile.run("reachable_plots(g, 1000)")

    with open("input.txt") as fh:
        lines = [l.strip() for l in fh]
        g = Grid(lines)
        #print("Reachable in exactly 64 steps", reachable_plots(g, 64))
        ig = Grid(lines, infinite=True)
        print("Reachable in exactly 26501365 steps", reachable_plots_infinite(ig, 26501365))
