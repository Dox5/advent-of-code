import typing
import doctest
import bisect
import collections
import numpy as np
import itertools


class Inst(typing.NamedTuple):
    direction: str
    dist: int

    @classmethod
    def from_line(cls, s: str, real_mode=False):
        """
        >>> Inst.from_line("R 6 (#70c710)")
        Inst(direction='R', dist=6)
        >>> Inst.from_line("R 6 (#70c710)", real_mode=True)
        Inst(direction='R', dist=461937)
        """
        d, dist, cc = s.strip().split()

        if real_mode:
            cc = cc.strip("()#")
            if cc[-1] == "0":
                d = "R"
            elif cc[-1] == "1":
                d = "D"
            elif cc[-1] == "2":
                d = "L"
            elif cc[-1] == "3":
                d = "U"
            else:
                raise RuntimeError(d)
            dist = int(cc[0:5], 16)


        return Inst(direction=d,
                    dist=int(dist))
                    

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

    def __mul__(self, other: typing.Union[int, "Point"]) -> "Point":
        if not isinstance(other, Point):
            other = Point(other, other)

        return Point(self.x * other.x, self.y * other.y)


def neighbours_of(point, dimensions):
    x, y = point
    for n_x, n_y in [(x, y-1), (x-1, y), (x, y+1), (x+1, y)]:
        if n_x in dimensions[0] and n_y in dimensions[1]:
            yield (n_x, n_y)

def is_clockwise(steps):
    turn_bias = 0

    facing = steps[0]

    left_turns = set([
        ("U", "L"),
        ("L", "D"),
        ("D", "R"),
        ("R", "U")
    ])

    right_turns = set([
        ("U", "R"),
        ("R", "D"),
        ("D", "L"),
        ("L", "U")
    ])

    for f in steps[1:]:
        turn = (facing, f)

        # Did we turn left?
        if facing == f:
            # No turn
            pass
        elif turn in left_turns:
            turn_bias -= 1
        elif turn in right_turns:
            turn_bias += 1
        else:
            raise RuntimeError(f"Not a left or right turn: {turn}")

        facing = f

    # This should never happen for a loop...
    assert turn_bias != 0

    return turn_bias < 0

test_square = [
    Inst(direction="R", dist=2),
    Inst(direction="D", dist=2),
    Inst(direction="L", dist=2),
    Inst(direction="U", dist=2),
]

test_big_square = [
    Inst(direction="R", dist=4),
    Inst(direction="D", dist=4),
    Inst(direction="L", dist=4),
    Inst(direction="U", dist=4),
]

test_heart = [
    Inst(direction="R", dist=2),
    Inst(direction="D", dist=1),
    Inst(direction="R", dist=2),
    Inst(direction="D", dist=2),
    Inst(direction="L", dist=4),
    Inst(direction="U", dist=3),
]

class DigMap2:
    _adjust = {
        "U": Point(x= 0, y=-1),
        "R": Point(x= 1, y= 0),
        "D": Point(x= 0, y= 1),
        "L": Point(x=-1, y= 0),
    }

    def __init__(self, verticies: list[Point], perim):
        self._verticies = verticies
        self._perim = perim


    @classmethod
    def from_instructions(cls, instructions: list[Inst]):

        last = instructions[0].direction
        digger = Point(0, 0)

        # 0,0 should always be a vertex
        points = [digger]

        for i in instructions:
            if i.direction != last:
                # Changed direction, so this is a vertex
                points.append(digger)
            offset = DigMap2._adjust[i.direction] * i.dist
            #print("@", digger, "step", i, "offset", offset)
            digger += offset

            last = i.direction

        perim = sum([i.dist for i in instructions])

        return DigMap2(points, perim)

    def total_area(self):
        """
        >>> DigMap2.from_instructions(test_square).total_area()
        9

        >>> DigMap2.from_instructions(test_big_square).total_area()
        25

        >>> DigMap2.from_instructions(test_heart).total_area()
        18
        """
        segments = itertools.chain(itertools.pairwise(self._verticies), [(self._verticies[-1], self._verticies[0])])

        matracies = [
            np.matrix([[p0.x, p1.x], [p0.y, p1.y]]) for p0, p1 in segments
        ]

        twice_area = sum([np.linalg.det(m) for m in matracies])

        # Fix because area doesn't fully include our perimeter
        return round(self._perim/2 + abs(twice_area/2) + 1)




class DigMap:
    _adjust = {
        "U": Point(x= 0, y=-1),
        "R": Point(x= 1, y= 0),
        "D": Point(x= 0, y= 1),
        "L": Point(x=-1, y= 0),
    }

    def __init__(self, dug, path):
        self._dug_cells = sorted(dug)
        self._x_bound = range(0, 0)
        self._y_bound = range(0, 0)
        self._path = path

        for c in self._dug_cells:
            if c.x < self._x_bound.start:
                self._x_bound = range(c.x, self._x_bound.stop)

            if c.x >= self._x_bound.stop:
                self._x_bound = range(self._x_bound.start, c.x+1)

            if c.y < self._y_bound.start:
                self._y_bound = range(c.y, self._y_bound.stop)

            if c.y >= self._y_bound.stop:
                self._y_bound = range(self._y_bound.start, c.y+1)

    @property
    def bounds(self):
        return self._x_bound, self._y_bound

    def __getitem__(self, key:typing.Union[Point, tuple[int, int]]):
        if key in self:
            return "#"
        else:
            return "."

    def __contains__(self, key: typing.Union[Point, tuple[int, int]]):
        if not isinstance(key, Point):
            key = Point(*key)

        found = bisect.bisect_left(self._dug_cells, key)
        return found < len(self._dug_cells) and self._dug_cells[found] == key


    def to_lines(self):
        lines = []
        for y in self._y_bound:
            line = []
            for x in self._x_bound:
                line.append(self[(x, y)])
            lines.append("".join(line))

        return lines

    def _is_clockwise(self):
        turn_bias = 0

        facing = self._path[0]

        left_turns = set([
            ("U", "L"),
            ("L", "D"),
            ("D", "R"),
            ("R", "U")
        ])

        right_turns = set([
            ("U", "R"),
            ("R", "D"),
            ("D", "L"),
            ("L", "U")
        ])

        for f in self._path[1:]:
            turn = (facing, f)

            # Did we turn left?
            if facing == f:
                # No turn
                pass
            elif turn in left_turns:
                turn_bias -= 1
            elif turn in right_turns:
                turn_bias += 1
            else:
                raise RuntimeError(f"Not a left or right turn: {turn}")

            facing = f

        # This should never happen for a loop...
        assert turn_bias != 0

        return turn_bias > 0

    def total_area(self):
        # Always deal with a clockwise path for ease
        path = self._path if self._is_clockwise() else reversed(self._path)

        inside = set()

        right_of = {
            "U": Point(x= 1, y= 0),
            "R": Point(x= 0, y= 1),
            "D": Point(x=-1, y= 0),
            "L": Point(x= 0, y=-1),
        }

        # Walk the path, noting any non-hash tiles that are on the right side

        digger = Point(0,0)

        for step in path:
            # Which side should we mark
            mark_offset = right_of[step]

            # Mark that side at our current position (essentially turn, mark
            # then move)
            mark = digger + mark_offset
            if self[mark] == ".":
                inside.add(digger + mark_offset)

            # Now move
            offset = DigMap._adjust[step]
            digger += offset

        # inside is now a set of points within the loop - not _all_ the points
        # however. Expand them all out
        q = collections.deque(inside)
        expanded = set()
        while True:
            try:
                e = q.pop()
                print(e)
            except IndexError:
                break

            if e in expanded:
                # Already seen this node
                continue

            q.extend(n for n in neighbours_of(e, self.bounds) if self[n] == ".")

            expanded.add(e)

        print("-"*80)


        for y in self._y_bound:
            for x in self._x_bound:
                p = Point(x,y)
                print("x" if p in expanded else self[p], end="")
            print()



        return len(expanded) + len(self._dug_cells)



    @classmethod
    def from_instructions(cls, inst: typing.Iterable[Inst]):
        dug = []
        # Path taken to make the perimeter
        path = []

        digger = Point(x=0, y=0)

        for i in inst:
            offset = DigMap._adjust[i.direction]

            for _ in range(i.dist):
                path.append(i.direction)
                digger += offset
                dug.append(digger)

        return DigMap(dug, path)

def parse_instructions(lines: list[str], real_mode=False):
    return [Inst.from_line(l.strip(), real_mode) for l in lines]


example_1 = [
  "R 6 (#70c710)",
  "D 5 (#0dc571)",
  "L 2 (#5713f0)",
  "D 2 (#d2c081)",
  "R 2 (#59c680)",
  "D 2 (#411b91)",
  "L 5 (#8ceee2)",
  "U 2 (#caa173)",
  "L 1 (#1b58a2)",
  "U 2 (#caa171)",
  "R 2 (#7807d2)",
  "U 3 (#a77fa3)",
  "L 2 (#015232)",
  "U 2 (#7a21e3)",
]

if __name__ == "__main__":
    doctest.testmod()

    print("Example")

    digmap = DigMap2.from_instructions(parse_instructions(example_1))

    print("Area dug", digmap.total_area())
    print("-" * 80)

    with open("input.txt") as fh:
        lines = fh.readlines()
        print("part a")
        digmap = DigMap2.from_instructions(parse_instructions(lines))
        print("Area dug", digmap.total_area())
        print("-" * 80)

        print("part b")
        digmap = DigMap2.from_instructions(parse_instructions(lines, real_mode=True))
        print("Area dug", digmap.total_area())
        print("-" * 80)

