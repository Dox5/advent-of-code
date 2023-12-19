import doctest
import typing
import io

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

def extract_rocks(lines: list[str]):
    """
    >>> extract_rocks(["#..", ".#."])
    [Point(x=0, y=0), Point(x=1, y=1)]
    """
    rocks = []

    for y, line in enumerate(lines):
        for x, s in enumerate(line):
            if s == "#":
                rocks.append(Point(x, y))

    return rocks

def cols(rocks, width):
    col_rock_pos = []

    for x in range(0, width):
        # Make a tuple out of the sorted y values for all the rocks in this
        # column
        col_rock_pos.append(tuple(sorted(r.y for r in rocks if r.x == x)))

    return col_rock_pos

def rows(rocks, height):
    row_rock_pos = []

    for y in range(0, height):
        # Make a tuple out of the sorted y values for all the rocks in this
        # column
        row_rock_pos.append(tuple(sorted(r.x for r in rocks if r.y == y)))

    return row_rock_pos

example_1 = [
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
]

example_2 = [
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#",
]

broken_1 = [
"##..##.",
"#.#.##.",
"#.#.##.",
"##..##.",
"###.#.#",
".#.#..#",
"..#...#",
"..##...",
".##...#",
".##.#.#",
".##.#.#",
".##.#.#",
"..##...",
]

def compare(pat_a, pat_b):
    #print("Compare", pat_a, pat_b)
    if pat_a == pat_b:
        # Match, no smudge needed
        return True, False

    diff = set(pat_a) ^ set(pat_b)
    #print("Checking smudge between", pat_a, pat_b, "determined", diff)
    if len(diff) == 1:
        # Fixable with a single smudge!
        return True, True

    return False, False


def check_for_mirrors(pattern: list[tuple[int]], allow_smudge=False) -> typing.Optional[int]:
    """
    >>> check_for_mirrors(rows(extract_rocks(example_1), len(example_1)))

    >>> check_for_mirrors(cols(extract_rocks(example_1), len(example_1[0])))
    (4, 5, False)

    >>> check_for_mirrors(rows(extract_rocks(example_1), len(example_1)), True)
    (2, 3, True)

    >>> check_for_mirrors(rows(extract_rocks(example_2), len(example_2)))
    (3, 4, False)

    >>> check_for_mirrors(rows(extract_rocks(example_2), len(example_2)), True)
    (0, 1, True)

    >>> check_for_mirrors(cols(extract_rocks(example_2), len(example_2[0])))
    """


    for i0, (r0, r1) in enumerate(zip(pattern, pattern[1:])):
        match, used_smudge = compare(r0, r1)
        if match and (allow_smudge or not used_smudge):
            #print("Found potential start at", i0)
            # Possible start here
            # walk backwards to see if this is really a start
            i1 = i0+1
            max_steps=min(i0, len(pattern) - i1 - 1)
            match = True

            for offset in range(1, max_steps+1):
                match, ns = compare(pattern[i0 - offset], pattern[i1+offset])
                #print("Checking offset", offset, pattern[i0 - offset], pattern[i1+offset], match, ns)
                
                # Did we only match because we smudged but we aren't allowed
                if match and (ns and not allow_smudge):
                    #print("Not a match without smudge and not allowed to smudge")
                    match = False
                    break
                elif all([match, ns, allow_smudge, used_smudge]):
                    #print("Match needed a smudge, but it's been used")
                    # Matched, allowed to smudge but already used it!
                    match = False
                    break
                elif not match:
                    # Just a boring non-match (not smudgable)
                    break


                # Keep track of if we've used the smudge
                used_smudge |= ns

            # Only return matches with smudges if in smudge mode
            if match and allow_smudge == used_smudge:
                return i0, i1, used_smudge

    return None

def ahead_of_mirror(lines: list[str], allow_smudge=False):
    """
    >>> ahead_of_mirror(example_1)
    5
    >>> ahead_of_mirror(example_1, True)
    300

    >>> ahead_of_mirror(example_2)
    400
    >>> ahead_of_mirror(example_2, True)
    100

    >>> ahead_of_mirror(broken_1, True)
    1000
    """
    rocks = extract_rocks(lines)

    horr = check_for_mirrors(rows(rocks, len(lines)), allow_smudge)
    if horr is not None:
        y0, y1, used_smudge = horr
        if not allow_smudge or (allow_smudge and used_smudge):
            return 100 * (y0 + 1)

    vert = check_for_mirrors(cols(rocks, len(lines[0])), allow_smudge)
    if vert is not None:
        x0, x1, used_smudge = vert
        if not allow_smudge or (allow_smudge and used_smudge):
            return x0 + 1

            

def summarise_patterns(fh, allow_smudge=False):
    lines = []

    total = 0

    for line in fh:
        line = line.strip()
        if line == "":
            # Do all the stuff
            t = ahead_of_mirror(lines, allow_smudge)
            total += t
            lines = []
        else:
            lines.append(line)

    if lines != []:
        total += ahead_of_mirror(lines, allow_smudge)

    return total



if __name__ == "__main__":
    doctest.testmod()

    with open("input.txt") as fh:
        print("total:", summarise_patterns(fh))

    with open("input.txt") as fh:
        print("total (smudge):", summarise_patterns(fh, True))

