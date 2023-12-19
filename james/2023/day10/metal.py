import enum
import doctest
import networkx
import itertools
import collections

from pprint import pprint

examples = [

[
    ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    ".....",
],

[
"-L|F7",
"7S-7|",
"L|7||",
"-L-J|",
"L|-JF",
],

[
"...........",
".S-------7.",
".|F-----7|.",
".||.....||.",
".||.....||.",
".|L-7.F-J|.",
".|..|.|..|.",
".L--J.L--J.",
"...........",
],

[
".F----7F7F7F7F-7....",
".|F--7||||||||FJ....",
".||.FJ||||||||L7....",
"FJL7L7LJLJ||LJ.L-7..",
"L--J.L7...LJS7F-7L7.",
"....F-J..F7FJ|L7L7L7",
"....L7.F7||L7|.L7L7|",
".....|FJLJ|FJ|F7|.LJ",
"....FJL-7.||.||||...",
"....L---J.LJ.LJLJ...",
]

]


class Conn(enum.Enum):
    North = enum.auto()
    East  = enum.auto()
    South = enum.auto()
    West  = enum.auto()

    @property
    def opposite(self):
        """
        >>> Conn.North.opposite
        <Conn.South: 3>
        """
        if self == Conn.North:
            return Conn.South
        elif self == Conn.East:
            return Conn.West
        elif self == Conn.South:
            return Conn.North
        elif self == Conn.East:
            return Conn.West


def from_section(c: str) -> set[Conn]:
    if c == "|":
        return set([Conn.North, Conn.South])
    elif c == "-":
        return set([Conn.East, Conn.West])
    elif c == "L":
        return set([Conn.North, Conn.East])
    elif c == "J":
        return set([Conn.North, Conn.West])
    elif c == "7":
        return set([Conn.South, Conn.West])
    elif c == "F":
        return set([Conn.South, Conn.East])
    elif c == ".":
        return set()
    elif c == "S":
        return set([Conn.North, Conn.East, Conn.South, Conn.West])
    else:
        raise ValueError(f"What is {c}")


def neighbours_of(point, dimensions):
    x, y = point
    for n_x, n_y in [(x, y-1), (x-1, y), (x, y+1), (x+1, y)]:
        if n_x in dimensions[0] and n_y in dimensions[1]:
            yield (n_x, n_y)

def south_east_neighbours(point, dimensions):
    x, y = point
    for n_x, n_y in [(x, y+1), (x+1, y)]:
        if n_x in dimensions[0] and n_y in dimensions[1]:
            yield (n_x, n_y)

def neighbour(point, dimensions):
    x, y = point
    for n_x, n_y in [(x, y+1), (x+1, y)]:
        if n_x in dimensions[0] and n_y in dimensions[1]:
            yield (n_x, n_y)




class Sketch:
    # 0,0 is top left
    def __init__(self, lines: str):
        self._lines = lines
        # Find the origin

        self._dim = (range(0, len(lines[0])), range(0, len(lines)))

        for y, line in enumerate(lines):
            try:
                x = line.index("S")
                self._origin = (x, y)
                break
            except ValueError:
                pass

    @property
    def origin(self):
        return self._origin

    @property
    def dimensions(self):
        return self._dim

    def __getitem__(self, loc) -> set[Conn]:
        try:
            x, y = loc
        except ValueError:
            raise TypeError("Location should be tuple-like of (x, y)")

        try:
            line = self._lines[y]
            return from_section(line[x])
        except IndexError:
            raise IndexError(f"{loc} is not in the sketch")

    def __contains__(self, loc):
        x, y = loc
        width, height = self.dimensions
        return x in width and y in height

    def _check_conn(self, node, neigh_loc, conn_side: Conn):
        if conn_side not in node:
            return False

        try:
            neigh = self[neigh_loc]
            if conn_side.opposite in neigh:
                # Found a connection
                return True
        except IndexError:
            pass

        return False

    @property
    def graph(self):
        width, height = self.dimensions

        g = networkx.Graph()



        # For each no
        for y in height:
            for x in width:
                # Only need to worry about south and east as we start in top
                # left (0, 0)
                node = self[(x, y)]
                g.add_node((x, y), data={"pipe": node})

                if self._check_conn(node, (x, y+1), Conn.South):
                    g.add_edge((x, y), (x, y+1))

                if self._check_conn(node, (x+1, y), Conn.East):
                    g.add_edge((x, y), (x+1, y))

        return g

def step_to_dir(f, t):
    f_x, f_y = f
    t_x, t_y = t

    change = (t_x - f_x, t_y - f_y)

    if change == (-1, 0):
        return Conn.West
    elif change == (1, 0):
        return Conn.East
    elif change == (0, -1):
        return Conn.North
    elif change == (0, 1):
        return Conn.South
    else:
        raise ValueError(f"Nope {f} -> {t} = {change}")

def counterclockwise(cycle) -> bool:
    turn_bias = 0

    facing = step_to_dir(*cycle[0])

    for f, t in cycle[1:]:
        new_dir = step_to_dir(f, t)

        if new_dir == facing:
            # Going in a straight line
            #print(f"{f} -> {t} is straight")
            continue
        
        if new_dir == facing.opposite:
            raise ValueError("Cannot do 180deg turn!")

        b = 0

        if new_dir == Conn.North:
            if facing == Conn.East:
                b =  1
            else:
                b = -1

        elif new_dir == Conn.East:
            if facing == Conn.South:
                b =  1
            else:
                b = -1

        elif new_dir == Conn.South:
            if facing == Conn.West:
                b =  1
            else:
                b = -1

        elif new_dir == Conn.West:
            if facing == Conn.North:
                b =  1
            else:
                b = -1

        #print(f"took step {f} -> {t}, which is {facing} -> {new_dir} bias adjust is {b}")
        turn_bias += b
        facing = new_dir

    # This shouldn't be possible with a cycle!
    assert turn_bias != 0

    #print(turn_bias)
    return turn_bias > 0




def intercept_at_steps(s: Sketch) -> int:
    """
    >>> intercept_at_steps(Sketch(examples[1]))
    4
    """
    g = s.graph
    
    cycle = networkx.find_cycle(g, s.origin)
    return int(len(cycle) / 2)

def point_add(a, b):
    ax, ay = a
    bx, by = b
    return ax+bx, ay+by

left_of = {
    Conn.North: (-1,  0),
    Conn.East:  ( 0, -1),
    Conn.South: (+1,  0),
    Conn.West:  ( 0, +1),
}

def enclosed(s: Sketch) -> int:
    """
    >>> enclosed(Sketch(examples[2]))
    4

    >>> enclosed(Sketch(examples[3]))
    8
    """
    g = s.graph
    cycle = networkx.find_cycle(g, s.origin)

    # Always work with a counterclockwise cycle. Left hand side will always be
    # 'inside' in this case
    if not counterclockwise(cycle):
        print("ono")
        cycle = [ tuple(reversed(c)) for c in reversed(cycle)]
        assert counterclockwise(cycle)

    in_cycle = set(itertools.chain.from_iterable((n for n in c) for c in cycle))

    # Now replace anything in our sketch outside our cycle pipe
    new_lines = []
    for y, line in enumerate(s._lines):
        new_line = []
        for x, n in enumerate(line):
            if (x, y) in in_cycle:
                # Part of our cycle - keep it
                new_line.append(n)
            else:
                # Not part of the cycle, overwrite it
                new_line.append(".")
        new_lines.append("".join(new_line))

    # sc is s but with everything not in our cycle replaced by '.'
    sc = Sketch(new_lines)

    enc = set()
    for f, t in cycle:
        facing = step_to_dir(f, t)
        walk_step = left_of[facing]

        # Make sure we touch any islands
        check = point_add(t, walk_step)
        if sc[check] == set():
            enc.add(check)

        check = point_add(f, walk_step)
        if sc[check] == set():
            enc.add(check)

    print(len(enc))
    expanded = set()
    # We should have touched every island once - expand those selections out
    q = collections.deque(enc)
    while True:
        try:
            e = q.pop()
        except IndexError:
            break

        print(e)
        if e in expanded:
            # Already seen this node
            continue

        q.extend(n for n in neighbours_of(e, sc.dimensions) if sc[n] == set())

        expanded.add(e)

    assert len(expanded) >= len(enc)

    for y, line in enumerate(sc._lines):
        k = ['I' if (x, y) in expanded else s for x, s in enumerate(line)]
        print("".join(k))


    return len(expanded)









if __name__ == "__main__":
    doctest.testmod()


    with open("input.txt") as fh:
        lines = [l.strip() for l in fh.readlines()]
        s = Sketch(lines)
        print("Intercept at", intercept_at_steps(s))
        print("Enclosed", enclosed(s))
