import typing
import doctest
import itertools
import collections
from pprint import pprint

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

class Beam(typing.NamedTuple):
    _offset = {
        "N": Point(x= 0, y=-1),
        "E": Point(x= 1, y= 0),
        "S": Point(x= 0, y= 1),
        "W": Point(x=-1, y= 0),
    }
    loc: Point
    direction: str # N, E, S, W

    def deflect(self, direction: str):
        new_loc = self.loc + Beam._offset[direction]

        return Beam(loc=new_loc, direction=direction)

class Director(typing.NamedTuple):
    loc: Point
    kind: str # . / \ - |

    def direct(self, b: Beam) -> list[Beam]:
        if self.kind == ".":
            # 'deflect' in the same direction to move the beam along
            return [b.deflect(b.direction)]

        if b.direction == "N":
            if self.kind == "/":
                return [b.deflect("E")]
            elif self.kind == "\\":
                return [b.deflect("W")]
            elif self.kind == "-":
                return [b.deflect("W"), b.deflect("E")]
            elif self.kind == "|":
                return [b.deflect("N")]

        elif b.direction == "E":
            if self.kind == "/":
                return [b.deflect("N")]
            elif self.kind == "\\":
                return [b.deflect("S")]
            elif self.kind == "-":
                return [b.deflect("E")]
            elif self.kind == "|":
                return [b.deflect("N"), b.deflect("S")]

        elif b.direction == "S":
            if self.kind == "/":
                return [b.deflect("W")]
            elif self.kind == "\\":
                return [b.deflect("E")]
            elif self.kind == "-":
                return [b.deflect("E"), b.deflect("W")]
            elif self.kind == "|":
                return [b.deflect("S")]

        elif b.direction == "W":
            if self.kind == "/":
                return [b.deflect("S")]
            elif self.kind == "\\":
                return [b.deflect("N")]
            elif self.kind == "-":
                return [b.deflect("W")]
            elif self.kind == "|":
                return [b.deflect("N"), b.deflect("S")]

        else:
            raise RuntimeError(f"What is direction {b.direction}")



class Board:
    def __init__(self, grid: list[str]):
        self._height = len(grid)
        self._width = len(grid[0])
        self._cells = list(itertools.chain.from_iterable([
            [Director(loc=Point(x, y), kind=k) for x, k in enumerate(row)] for y, row in enumerate(grid)
        ]))

    def __getitem__(self, key: typing.Union[Point, tuple[int, int]]):
        x, y = key
        return self._cells[x + y * self._width]

    def __contains__(self, key: typing.Union[Point, tuple[int, int]]):
        x, y = key
        return x in range(0, self._width) and y in range(0, self._height)

example_1 = Board([
".|...\\....",
"|.-.\\.....",
".....|-...",
"........|.",
"..........",
".........\\",
"..../.\\\\..",
".-.-/..|..",
".|....-|.\\",
"..//.|....",
])


def show_paths(board: Board, beams: set[Beam]):
    icons = { }

    for b in beams:
        icons.setdefault(b.loc, set()).add(b.direction)

    for y in range(0, board._height):
        line = []
        for x in range(0, board._width):
            d = board[(x, y)]
            if d.kind == ".":
                passing = icons.get(Point(x, y), "")
                if passing == "":
                    line.append(".")
                elif len(passing) > 1:
                    line.append(str(len(passing)))
                else:
                    c = next(iter(passing))
                    line.append({"N": "^", "E": ">", "S": "v", "W": "<"}[c])
            else:
                line.append(d.kind)
        print("".join(line))

def energised(board: Board, start_beam=Beam(loc=Point(0, 0), direction="E")):
    """
    >>> energised(example_1)
    46
    """

    seen = set()

    # Always start in the top left heading east
    explore = collections.deque([start_beam])

    while True:
        try:
            b = explore.popleft()
        except IndexError:
            # No more beams to explore
            break

        # Keep exploring this path until we loop or fall off an edge
        while b.loc in board and b not in seen:
            seen.add(b)
            director = board[b.loc]
            # Find the next paths (either 1 or 2)
            b, *extras = director.direct(b)
            # Queue the one we're not handling right now
            explore.extend(extras)

    #show_paths(board, seen)

    energised = set([s.loc for s in seen])
    return len(energised)


if __name__ == "__main__":
    doctest.testmod()

    with open("input.txt") as fh:
        lines = [l.strip() for l in fh]
        board = Board(lines)
        print("Energised cells", energised(board))

        print("Finding best start")

        perimeter = itertools.chain(
                # South from top
                [Beam(loc=Point(x, 0), direction="S") for x in range(0, board._width)],

                # North from bottom
                [Beam(loc=Point(x, board._height-1), direction="N") for x in range(0, board._width)],

                # East from left
                [Beam(loc=Point(0, y), direction="E") for y in range(0, board._height)],

                # West from Right
                [Beam(loc=Point(board._width-1, y), direction="W") for y in range(0, board._height)])

        best = max(energised(board, start) for start in perimeter)
        print("Most energised possible", best)






