import doctest
import typing
import itertools
import collections
from pprint import pprint
import copy

Vec = tuple[int, int, int]
X = 0
Y = 1
Z = 2

def _to_vec(s) -> Vec:
    return tuple(int(a.strip()) for a in s.split(","))

class Brick:
    def __init__(self, start: Vec, end: Vec, label: typing.Any = None):
        self._loc = start
        self._label = label

        assert end[Z] >= start[Z]

        (x1, y1, z1) = start
        (x2, y2, z2) = end

        dx = x2-x1
        dy = y2-y1
        dz = z2-z1

        self._dim = (dx, dy, dz)




    @property
    def dim(self):
        return self._dim

    @property
    def loc(self):
        return self._loc

    @property
    def label(self):
        return self._label

    @property
    def occupies(self) -> typing.Iterable[Vec]:
        """
        >>> list(Brick.parse("0,0,4~0,2,4").occupies)
        [(0, 0, 4), (0, 1, 4), (0, 2, 4)]
        """
        w, d, h = self.dim
        x, y, z = self.loc

        for (ox, oy, oz) in itertools.product(range(0, w+1), range(0, d+1), range(0, h+1)):
            yield (x+ox, y+oy, z+oz)

    def translate(self, tx, ty, tz):
        """
        >>> b = Brick.parse("0,0,4~0,2,4"); b.translate(1, 2, -1); b.loc
        (1, 2, 3)
        """
        x, y, z = self._loc
        self._loc = (x+tx, y+ty, z+tz)


    @classmethod
    def parse(cls, str_brick: str, label=None):
        """
        >>> Brick.parse("0,0,4~0,2,4").dim
        (0, 2, 0)
        >>> Brick.parse("0,0,4~0,2,4").loc
        (0, 0, 4)
        """
        start, end = str_brick.split("~")

        return cls(_to_vec(start), _to_vec(end), label=label)


def settle(bricks: list[Brick]):
    # Work through bricks by z height (upward)
    bricks = sorted(bricks, key=lambda b: b.loc[Z])

    z_map = dict()

    for b in bricks:
        if b.loc[Z] > 1:
            # Not on the floor so check for supports
            highest_support = max(z_map.get((ox, oy), 0) for (ox, oy, _) in b.occupies)

            # Check if we're on a support...
            if highest_support < (b.loc[Z] - 1):
                # Need to move down
                distance = b.loc[Z] - highest_support - 1
                b.translate(0, 0, -distance)

        # Now we can update the z_map with the new heights
        for (ox, oy, oz) in b.occupies:
            pos = (ox, oy)
            z = z_map.get(pos, 0)
            z_map[pos] = max(z, oz)


    # These are now settled down into place



def dump_bricks(bricks: list[Brick]):
    bricks = sorted(bricks, key=lambda b: b.loc[Z], reverse=True)

    width = max(b.loc[X]+b.dim[X] for b in bricks) + 1
    height = max(b.loc[Y]+b.dim[Y] for b in bricks) + 1
    depth = max(b.loc[Z]+b.dim[Z] for b in bricks) + 1

    grid = [ "." ] * (width * depth * height)
    xstep = 1
    ystep = width
    zstep = width*height

    for b in bricks:
        for x, y, z in b.occupies:
            idx = x*xstep + y*ystep + z*zstep
            grid[idx] = b.label


    for z in range(depth-1, -1, -1):
        for y in range(0, height):
            start = y*ystep + z*zstep





def make_supports_list(bricks: list[Brick]):
    # Must be sorted by z
    assert all(l.loc[Z] <= r.loc[Z] for l, r in itertools.pairwise(bricks))


    # x, y => (z, brick_n)
    z_map = dict()

    # Create all of our sets for each brick
    supports = [set() for _ in range(len(bricks))]

    for n, b in enumerate(bricks):
        for (bx, by, bz) in b.occupies:
            z, support = z_map.get((bx, by), (0, None))

            # Does this support this brick?
            if z == (bz - 1) and support is not None:
                # Add this brick to the supports set for the below
                supports[support].add(n)

        # Now remember this as a support for the future
        for (bx, by, bz) in b.occupies:
            z, _ = z_map.get((bx, by), (0, None))
            if z < bz:
                # Got a higher point now that will be the support
                z_map[(bx, by)] = (bz, n)
    return supports




def part_a(bricks: list[Brick]):
    # First simulate some gravity to settle all the bricks
    settle(bricks)
    bricks.sort(key=lambda b: b.loc[Z])
    #dump_bricks(bricks)


    supports = make_supports_list(bricks)


    layers = collections.defaultdict(list)
    can_be_elim = []
    for n, b in enumerate(bricks):
        layers[b.loc[Z] + b.dim[Z]].append(n)

    for _, layer in layers.items():

        for i in range(len(layer)):
            i_sup = supports[layer[i]]
            others_support = set()

            for j in range(len(layer)):
                if j == i:
                    # Don't self add
                    continue
                j_sup = supports[layer[j]]
                others_support |= j_sup

            if others_support & i_sup == i_sup:
                # Everything supported by this brick has another support, so it
                # can be dissolved
                can_be_elim.append(layer[i])

    return can_be_elim




def part_b(bricks: list[Brick]):
    settle(bricks)
    bricks.sort(key=lambda b: b.loc[Z])

    supports = make_supports_list(bricks)

    # Invert the list for this bit
    supported_by = [set() for _ in range(len(bricks))]
    for n, s in enumerate(supports):
        for q in s:
            supported_by[q].add(n)

    # Any that are currently the empty list must be supported by the ground so
    # add a special element to stop them being deleted
    for s in supported_by:
        if s == set():
            s.add("G")


    # Now ready to start counting..
    total_falling = 0
    for disintegrate in range(len(supported_by)):
        # We're going to be mutating this a heap so take a deepcopy!
        working = copy.deepcopy(supported_by)

        # Now eliminate the initial one (this doesn't count as falling). Update
        # the set of supports. Anything with 0 supports now will start to fall
        working[disintegrate] = None
        for w in range(disintegrate+1, len(working)):
            if working[w] is not None:
                working[w].discard(disintegrate)

        # Falling loop!
        made_change = True
        while made_change:
            made_change = False
            # Look for anything with no supports, due to ordering can just check
            # from disintegrate onwards

            for i in range(disintegrate+1, len(working)):
                if working[i] == set():
                    # This block has no supports
                    total_falling += 1
                    # Prevent us dealing with this one again
                    working[i] = None

                    # Remove this as a support to trigger any further drops
                    for q in range(disintegrate+1, i):
                        if working[q] is not None:
                            assert i not in working[q], f"{q}, {working[q]}, {i}, {working}"

                    for j in range(i+1, len(working)):
                        if working[j] is not None and i in working[j]:
                            working[j].remove(i)
                            made_change=True


    return total_falling
                    






if __name__ == "__main__":
    doctest.testmod()

    example_bricks = [ Brick.parse(s, label=n) for n, s in
        enumerate([ "1,0,1~1,2,1",
          "0,0,2~2,0,2",
          "0,2,3~2,2,3",
          "0,0,4~0,2,4",
          "2,0,5~2,2,5",
          "0,1,6~2,1,6",
          "1,1,8~1,1,9",])]

    print("(example) Can eliminate", part_a(example_bricks))
    print("(example) Chain sum", part_b(example_bricks))

    with open("input.txt") as fh:
        bricks = [Brick.parse(l.strip(), label=n) for n, l in enumerate(fh)]
        print("Can eliminate", len(part_a(bricks)))

    with open("input.txt") as fh:
        bricks = [Brick.parse(l.strip(), label=n) for n, l in enumerate(fh)]
        print("Chain sum", part_b(bricks))
