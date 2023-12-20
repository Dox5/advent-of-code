import typing
import doctest
import itertools
import queue
from dataclasses import dataclass, field

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

    def __mul__(self, other: typing.Union[int, "Point"]) -> "Point":
        if not isinstance(other, Point):
            other = Point(other, other)

        return Point(self.x * other.x, self.y * other.y)

def dist(a, b):
    """
    >>> dist(Point(0, 0), Point(7, 7)) > dist(Point(1, 0), Point(7, 7))
    True
    """
    return abs(a.x - b.x) + abs(a.y - b.y)

example_1 = [
    "2413432311323",
    "3215453535623",
    "3255245654254",
    "3446585845452",
    "4546657867536",
    "1438598798454",
    "4457876987766",
    "3637877979653",
    "4654967986887",
    "4564679986453",
    "1224686865563",
    "2546548887735",
    "4322674655533",
]

def dump(shortest_to, width, height):
    for y in range(height):
        for x in range(width):
            shortest = shortest_to.get(Point(x, y), -1)
            print(f"{shortest:4}", end="")
        print()


def dump_path(steps, width, height):
    steps = set(steps)
    for y in range(height):
        for x in range(width):
            s = "*" if Point(x, y) in steps else "."
            print(f"{s:>4}", end="")
        print()


class Grid:
    def __init__(self, rows: list[str]):
        self.width = len(rows[0])
        self.height = len(rows)

        self.cells = list(itertools.chain.from_iterable([int(c) for c in r] for r in rows))

    def __getitem__(self, loc):
        x, y = loc
        return self.cells[x + y * self.width]

    def __contains__(self, loc):
        x, y = loc
        return x in range(0, self.width) and y in range(0, self.height)

    def find_crucible_path(self, ultra=False):
        """
        #>>> Grid(example_1).find_crucible_path()
        102
        """
        start = Point(0, 0)
        end = Point(self.width-1, self.height-1)

        q = queue.PriorityQueue()
        q.put(Context(cost=0, path=[start], steps=[]).prio)

        shortest_to = { }

        shortest_known = None

        while not q.empty():
            v = q.get().item

            loc = v.path[-1]

            # Have we visited this before
            try:
                prev = shortest_to[v.uniq]
                if v.cost >= prev:
                    # Visited before but more expensive, so stop looking
                    continue
                elif shortest_known is not None and v.cost > shortest_known:
                    # No shorter paths possible
                    return shortest_known
            except KeyError:
                # No
                pass

            # Remember the cost to this node
            shortest_to[v.uniq] = v.cost


            # Have we found the end, by searching shortest paths first this must
            # be the shortest cost to here
            if loc == end:
                #pprint(v.path)
                #dump_path(v.path, self.width, self.height)
                #dump(shortest_to, self.width, self.height)
                if shortest_known is None or v.cost < shortest_known:
                    shortest_known = v.cost
            elif not ultra:
                # Queue neighbours based on path

                disallowed = set() # Points we can't step to
                try:
                    disallowed.add(v.path[-2]) # Can't do a 180
                except IndexError:
                    # Must be the first step
                    pass

                if len(v.steps) > 2:
                    if v.steps[-1] == v.steps[-2] and v.steps[-3] == v.steps[-1]:
                        # Deny next step in this direction
                        disallowed.add(loc + v.steps[-1])


                for offset in [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)]:
                    neigh = loc + offset
                    if neigh in self and neigh not in disallowed:
                        new_path = v.path.copy()
                        new_path.append(neigh)
                        new_steps = v.steps.copy()
                        new_steps.append(offset)
                        q.put(Context(cost=v.cost+self[neigh], path=new_path, steps=new_steps).prio)
            else:
                # Ultra buckets are being used
                
                same_dir_steps = 0
                for s in reversed(v.steps):
                    if s == v.steps[-1]:
                        same_dir_steps += 1
                    else:
                        break

                if same_dir_steps in range(1, 4):
                    # Can only move in line with these steps
                    neigh = loc + v.steps[-1]
                    # If this isn't in our grid then this branch can be culled
                    if neigh in self:
                        new_path = v.path.copy()
                        new_path.append(neigh)
                        new_steps = v.steps.copy()
                        new_steps.append(v.steps[-1])
                        q.put(Context(cost=v.cost+self[neigh], path=new_path, steps=new_steps).prio)
                else:
                    disallowed = set() # Points we can't step to
                    try:
                        disallowed.add(v.path[-2]) # Can't do a 180
                    except IndexError:
                        # Must be the first step
                        pass

                    # Cannot continue in this direction
                    if same_dir_steps == 10:
                      # Deny next step in this direction
                      disallowed.add(loc + v.steps[-1])


                    for offset in [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)]:
                        neigh = loc + offset
                        if neigh in self and neigh not in disallowed:
                            new_path = v.path.copy()
                            new_path.append(neigh)
                            new_steps = v.steps.copy()
                            new_steps.append(offset)
                            q.put(Context(cost=v.cost+self[neigh], path=new_path, steps=new_steps).prio)





        return shortest_known


        raise RuntimeError("uh oh")

@dataclass(order=True)
class PrioItem:
    prio: int
    item: typing.Any=field(compare=False)

@dataclass()
class Context:
    cost: int
    path: list[Point]
    steps: list[Point]

    @property
    def prio(self):
        return PrioItem(self.cost, self)

    @property
    def uniq(self):
        return tuple([self.path[-1], *self.steps[-10:]])


    @classmethod
    def make_start(cls, g, loc: Point) -> "Context":
        # Work out valid neighbours to walk to
        neighbours = set()
        for offset in [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)]:
            if offset in g:
                neighbours.add(loc + offset)

        return Context(loc=loc, to_explore=neighbours, cost=0)


    @classmethod
    def make(cls, g: Grid, loc: Point, p: list["Context"]) -> "Context":

        # Check we haven't gone straight for too long

        return Context(loc=loc, to_explore=neighbours, cost=p[-1].cost + g[loc])




if __name__ == "__main__":
    doctest.testmod()

    simple = [
        "1111",
        "1221",
        "1221",
        "1111",
    ]

    print(Grid(example_1).find_crucible_path(ultra=True))

    with open("input.txt") as fh:

        g = Grid([l.strip() for l in fh])
        print("Shortest path", g.find_crucible_path(ultra=True))

