import doctest
import typing
import io
import bisect

from pprint import pprint

example_a = """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"""

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

def load_galaxies(lines: list[str]) -> list[Point]:
    """
    >>> load_galaxies(["..#.", "...", "#..."])
    [Point(x=2, y=0), Point(x=0, y=2)]
    """
    galaxies = []
    for y, l in enumerate(lines):
        for x, s in enumerate(l):
            if s == "#":
                galaxies.append(Point(x=x, y=y))
    return galaxies

def find_gaps_in_seq(ns: list[int]) -> list[int]:
    """
    >>> find_gaps_in_seq([6, 0, 5, 1, 7, 9])
    [2, 3, 4, 8]
    """
    ordered = sorted(ns)
    got = set(ns)
    want = set(range(ordered[0], ordered[-1]+1))
    return sorted(want - got)

def expand(j: int, expanding: list[int], expand_amount: int) -> int:
    """
    >>> expand(2, [3, 5], 1)
    2
    >>> expand(4, [3, 5], 1)
    5
    >>> expand(6, [3, 5], 1)
    8


    >>> expand(2, [3, 5], 10)
    2
    >>> expand(4, [3, 5], 10)
    14
    >>> expand(6, [3, 5], 10)
    26
    """

    pos = bisect.bisect_left(expanding, j)
    return j + pos * expand_amount

# Fix the position of all the galaxies WRT expansion gaps
def expand_galaxies(galaxies: list[int], expand_amount: int) -> int:
    col_expand = find_gaps_in_seq([g.x for g in galaxies])
    row_expand = find_gaps_in_seq([g.y for g in galaxies])

    o = []
    for g in galaxies:
        updated = Point(
            x=expand(g.x, col_expand, expand_amount),
            y=expand(g.y, row_expand, expand_amount),
        )

        o.append(updated)
    return o


def point_to_point_dist(a: Point, b: Point) -> int:
    return max(a.x, b.x) - min(a.x, b.x) + max(a.y, b.y) - min(a.y, b.y)

def total_dist(fh, expand_amount=1):
    """
    >>> total_dist(io.StringIO(example_a))
    374

    >>> total_dist(io.StringIO(example_a), 10-1)
    1030

    >>> total_dist(io.StringIO(example_a), 100-1)
    8410
    """
    galaxies = load_galaxies(l.strip() for l in fh)
    galaxies = expand_galaxies(galaxies, expand_amount)

    total_dist = 0
    for i in range(0, len(galaxies)):
        for k in range(i+1, len(galaxies)):
            dist = point_to_point_dist(galaxies[i], galaxies[k])
            total_dist += dist

    return total_dist


    


if __name__ == "__main__":
    doctest.testmod()

    with open("input.txt") as fh:
        print("Total distance", total_dist(fh))

    with open("input.txt") as fh:
        print("Total distance (old)", total_dist(fh, 1000000-1))
