import typing
import itertools
import pathlib

class Coord(typing.NamedTuple):
    x: int
    y: int

class PartNumber(typing.NamedTuple):
    loc: Coord
    length: int 
    value: int

class Symbol(typing.NamedTuple):
    loc: Coord
    sym: str

def parse_table(lines: list[str]):
    """

    >>> parse_table(["467..114..", "...*......"])
    ([PartNumber(loc=Coord(x=0, y=0), length=3, value=467), PartNumber(loc=Coord(x=5, y=0), length=3, value=114)], [Symbol(loc=Coord(x=3, y=1), sym='*')])

    >>> parse_table(["..10-5"])
    ([PartNumber(loc=Coord(x=2, y=0), length=2, value=10), PartNumber(loc=Coord(x=5, y=0), length=1, value=5)], [Symbol(loc=Coord(x=4, y=0), sym='-')])
    
    >>> parse_table(["$.."])
    ([], [Symbol(loc=Coord(x=0, y=0), sym='$')])

    >>> parse_table([".$."])
    ([], [Symbol(loc=Coord(x=1, y=0), sym='$')])

    >>> parse_table(["..$"])
    ([], [Symbol(loc=Coord(x=2, y=0), sym='$')])
    """
    parts: list[PartNumber] = []
    symbols: list[Symbol] = []

    digits = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0"}

    for y, line in enumerate(lines):
        num_str = ""

        for x, c  in enumerate(line):
            if c in digits:
                num_str += c
            else:
                # Not a digit, generate part if we just stepped off of one
                if num_str != "":
                    # Remember: x is the coordinate PAST the end of the token,
                    # so don't need to adjust for the length / index difference
                    parts.append(PartNumber(loc=Coord(x=x-len(num_str), y=y), length=len(num_str), value=int(num_str)))

                    num_str = ""

                if c != ".":
                    # Treat anything else as a symbol
                    symbols.append(Symbol(loc=Coord(x=x, y=y), sym=c))

        # Hit end of the line, output any remaining number
        if num_str != "":
            # Remember: x is the coordinate PAST the end of the token,
            # so don't need to adjust for the length / index difference
            x = len(line)
            parts.append(PartNumber(loc=Coord(x=x-len(num_str), y=y), length=len(num_str), value=int(num_str)))

            num_str = ""


    return parts, symbols

def build_part_index(parts: list[PartNumber]) -> dict[Coord, PartNumber]:
    """Index all parts by the coordinates they occupy for fast lookup

    >>> build_part_index([PartNumber(loc=Coord(x=0, y=0), length=3, value=123), PartNumber(loc=Coord(x=4, y=5), length=1, value=7)])
    {Coord(x=0, y=0): PartNumber(loc=Coord(x=0, y=0), length=3, value=123), Coord(x=1, y=0): PartNumber(loc=Coord(x=0, y=0), length=3, value=123), Coord(x=2, y=0): PartNumber(loc=Coord(x=0, y=0), length=3, value=123), Coord(x=4, y=5): PartNumber(loc=Coord(x=4, y=5), length=1, value=7)}

    """

    num_lookup: list[tuple[Coord, PartNumber]] = itertools.chain.from_iterable(
        [(Coord(x=x, y=part.loc.y), part) for x in range(part.loc.x, part.loc.x + part.length)] 
            for part in parts
    )


    return dict(num_lookup)

def adjecent_cells(loc: Coord) -> typing.Iterable[Coord]:
    """
    >>> list(adjecent_cells(Coord(x=3, y=4)))
    [Coord(x=2, y=3), Coord(x=3, y=3), Coord(x=4, y=3), Coord(x=2, y=4), Coord(x=4, y=4), Coord(x=2, y=5), Coord(x=3, y=5), Coord(x=4, y=5)]

    >>> list(adjecent_cells(Coord(x=0, y=0)))
    [Coord(x=1, y=0), Coord(x=0, y=1), Coord(x=1, y=1)]
    """
    for y in [-1, 0, 1]:
        for x in [-1, 0, 1]:
            if x == 0 and y == 0:
                continue

            adj_x = loc.x + x
            adj_y = loc.y + y

            if adj_x < 0 or adj_y < 0:
                continue

            yield Coord(x=adj_x, y=adj_y)

def part_number_sum(parts: list[PartNumber], symbols: list[Symbol]) -> int:
    index = build_part_index(parts)

    parts_adjacent_to_symbol = set()

    for symbol in symbols:
        for adj in adjecent_cells(symbol.loc):
            try:
                parts_adjacent_to_symbol.add(index[adj])
            except KeyError:
                pass

    sanity = set()
    for p in parts_adjacent_to_symbol:
        sanity.add((p.loc.x, p.loc.y))

    assert len(sanity) == len(parts_adjacent_to_symbol)

    return sum(p.value for p in parts_adjacent_to_symbol)

            

def part_a(lines: list[str]) -> int:
    """
    >>> part_a(["467..114..", "...*......", "..35..633.", "......#...", "617*......", ".....+.58.", "..592.....", "......755.",  "...$.*....",  ".664.598.."])
    4361

    >>> part_a(["8&9$"])
    17

    >>> part_a(["$3...", ".100.", ".*.$."])
    103

    >>> part_a([".2..", ".*..", ".123."])
    125

    >>> part_a(["...%848"])
    848

    >>> part_a(["......489.420.459......*......*....*.....277.............%.........=...............................*..............746....512............%848"])
    848
    """
    parts, symbs = parse_table(lines)
    return part_number_sum(parts, symbs)

def part_b(lines: list[str]) -> int:
    """
    >>> part_b(["...5*8"])
    40

    >>> part_b(["467..114..", "...*......", "..35..633.", "......#...", "617*......", ".....+.58.", "..592.....", "......755.",  "...$.*....",  ".664.598.."])
    467835
    """
    parts, symbs = parse_table(lines)
    index = build_part_index(parts)

    ratio_sum = 0

    for gear in (x for x in symbs if x.sym == "*"):
        surrounding = set()
        for adj in adjecent_cells(gear.loc):
            try:
                surrounding.add(index[adj])
            except KeyError:
                pass

        if len(surrounding) == 2:
            a, b = surrounding

            ratio_sum += a.value * b.value

    return ratio_sum



if __name__ == "__main__":
    import doctest
    doctest.testmod()

    with pathlib.Path("input_a.txt").open("r") as f:
        lines = f.readlines()
        print("Sum of part numbers", part_a(l.strip() for l in lines))
        print("Sum of gear ratios", part_b(l.strip() for l in lines))


