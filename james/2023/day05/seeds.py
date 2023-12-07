import typing
import io
import os
import bisect
import collections
import itertools

example_input = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""

class Constraint(typing.NamedTuple):
    kind: str   # eg seed
    need: str  # eg soil

    kind_range: range
    need_range: range


def calc_new_id(i: int, c: Constraint) -> int:
    """
    >>> calc_new_id(3, Constraint(kind="", need="", kind_range=range(2, 5, 1), need_range=range(8, 10, 1)))
    9
    """
    offset = c.kind_range.index(i)
    return c.need_range.start + offset

def split_range_at(r: range, value: int) -> tuple[range, range]:
    """
    >>> split_range_at(range(0, 5), 0)
    (range(0, 1), range(1, 5))

    >>> split_range_at(range(5, 16), 10)
    (range(5, 11), range(11, 16))
    """

    # Check for sanity
    #assert value in r

    return (range(r.start, value+1), range(value+1, r.stop))

def project_range(frm: range, base: range, to: range) -> range:
    """
    >>> project_range(range(4, 8), range(0, 15), range(30, 45))
    range(34, 38)
    """
    #assert len(base) == len(to)
    #assert len(frm) <= len(base)

    new_start = to[base.index(frm.start)]
    return range(new_start, new_start + len(frm))

class Almanac:
    def __init__(self, initial_seeds: list[int] = [], conversions: dict[str, list[Constraint]] = []):
        self._initial_seeds = initial_seeds
        self._conversions = conversions

        for v in self._conversions.values():
            v.sort(key=lambda c: c.kind_range.start)

    def find_attr_of(self, got_kind: str, got_id: int, want_kind: str) -> int:
        """
        >>> read_almanac(io.StringIO(example_input)).find_attr_of("seed", 79, "location")
        82
        """
        current_kind = got_kind
        current_id = got_id

        while current_kind != want_kind:
            lookup = self._conversions[current_kind]
            pos = bisect.bisect_left(lookup, current_id, key=lambda e: e.kind_range.start)

            if pos == 0:
                # Not found, self map the ID
                current_kind = lookup[0].need
                continue

            # pos will be after the element we want to inspect
            check = lookup[pos-1]

            # Already checked the start for the ranges
            if current_id < check.kind_range.stop:
                # Found a mapping
                current_kind = check.need
                current_id = calc_new_id(current_id, check)
            else:
                # Not found, self map the ID
                current_kind = lookup[0].need

        return current_id

    def find_attr_from_range(self, got_kind: str, got_ranges: typing.Iterable[range], want_kind) -> typing.Iterable[range]:
        """
        >>> list(read_almanac(io.StringIO(example_input)).find_attr_from_range("seed", [range(79, 79+14)], "location"))
        []

        """
        q = collections.deque((got_kind, rng) for rng in got_ranges)

        while True:
            try:
                kind, rng = q.pop()
            except IndexError:
                #print("Queue empty")
                break
            
            if kind == want_kind:
                # Found a range for the desired kind
                #print("Found match", kind, rng)
                yield rng
                continue

            if len(q) > 500:
                print(len(q))

            #print("-"*16)
            #print("Looking up", kind, rng)
            # First find the table for kind -> need
            lookup = self._conversions[kind]
            need = lookup[0].need

            # Locate the entry that this range starts in (if any)
            pos = bisect.bisect_left(lookup, rng.start, key=lambda e: e.kind_range.start)
            #print("Found pos", pos, "(len =", len(lookup), ")")

            # Handle left most range

            # Only deal with the front of the range
            if pos != 0:
                # Possibly a previous entry to deal with - if we handle it
                # continue
                maybe_overlap = lookup[pos-1]
                #print("pos != 0 checking overlap with", maybe_overlap)

                # We could either start in this range or after it
                if rng.start in maybe_overlap.kind_range:
                    #print("Start in found range")
                    if rng.stop-1 in maybe_overlap.kind_range:
                        # Wholly within this the range, no splitting required
                        q.append((need, project_range(rng, maybe_overlap.kind_range, maybe_overlap.need_range)))
                    else:
                        # Partially covered

                        # Map this range chunk with the overlap
                        overlapping, remainder = split_range_at(rng, maybe_overlap.kind_range.stop-1)

                        # Overlapping range gets queued after being adjusted
                        q.append((need, project_range(overlapping, maybe_overlap.kind_range, maybe_overlap.need_range)))

                        # Need to keep working with remainder till it's gone!
                        q.append((kind, remainder))

                    continue


            # Check for overlap with pos, this one starts AFTER our rng so chop
            # into straightmap + remainder
            if pos < len(lookup):
                maybe_overlap = lookup[pos]
                #print("pos < len(lookup) checking overlap with", maybe_overlap)
                if maybe_overlap.kind_range.start == rng.start:
                    # Start at the same point, end at which ever ends first
                    first_stop = min(rng.stop, maybe_overlap.kind_range.stop)
                    overlap, remainder = split_range_at(rng, first_stop - 1)

                    q.append((kind, remainder))
                    q.append((need, project_range(overlap, maybe_overlap.kind_range, maybe_overlap.need_range)))

                elif maybe_overlap.kind_range.start-1 in rng:
                    # Overlapping tail

                    # Map this range chunk with the overlap
                    straight, remainder = split_range_at(rng, maybe_overlap.kind_range.start-1)

                    # Overlapping range gets queued after being adjusted
                    q.append((need, straight))

                    # Need to keep working with remainder till it's gone!
                    q.append((kind, remainder))

                else:
                    # No overlap at all - straight map whole range
                    q.append((need, rng))

                continue

            #print("No other case applied, so this is just a strait mapping")
            q.append((need, rng))
                



    @property
    def initial_seeds(self):
        return self._initial_seeds

    def __repr__(self):
        return f"Almanac(initial_seeds={self.initial_seeds}, conversions={self._conversions})"



def read_table(fh) -> typing.Iterable[Constraint]:
    header = fh.readline().strip()

    kind, _, need = header.split(" ")[0].split("-")

    for line in fh:
        line = line.strip()

        # Found end of table
        if line == "":
            break

        start_need, start_kind, length = [int(n) for n in line.split()]
        yield Constraint(kind=kind,
                         kind_range=range(start_kind, start_kind+length),
                         need=need,
                         need_range=range(start_need, start_need+length),
                         )

def read_almanac(fh) -> Almanac:
    # Starts with a line of all the seeds we care about
    seeds = [int(s) for s in fh.readline().split(":", 1)[1].split()]
    if fh.readline().strip() != "":
        raise RuntimeError("Expected blank line after seed list")

    # Remaining are now chunks of mappings
    mappings = dict()

    while True:
        try:
            m = list(read_table(fh))
            key = m[0].kind
            mappings[key] = m
        except ValueError:
            # This is really dumb but it's hard to tell your at the end of the
            # file?!
            break

    return Almanac(initial_seeds=seeds, conversions=mappings)

if __name__ == "__main__":
    import doctest
    doctest.testmod()

    almanac = read_almanac(io.StringIO(example_input))
    lowest_location = min(almanac.find_attr_of("seed", s, "location") for s in almanac.initial_seeds)
    print("(example) Lowest location:", lowest_location)
    seed_ranges = [range(almanac.initial_seeds[i], almanac.initial_seeds[i] + almanac.initial_seeds[i+1]) for i in range(0, len(almanac.initial_seeds), 2)]
    lowest_location_range = min(itertools.chain.from_iterable(almanac.find_attr_from_range("seed", seed_ranges, "location")))
    print("(example) Lowest location range:", lowest_location_range)


    with open("input.txt", "r") as fh:
        almanac = read_almanac(fh)
        lowest_location = min(almanac.find_attr_of("seed", s, "location") for s in almanac.initial_seeds)
        print("Lowest location:", lowest_location)

        seed_ranges = [range(almanac.initial_seeds[i], almanac.initial_seeds[i] + almanac.initial_seeds[i+1]) for i in range(0, len(almanac.initial_seeds), 2)]
        lowest_location_range = min(itertools.chain.from_iterable(almanac.find_attr_from_range("seed", seed_ranges, "location")))
        print("Lowest location range:", lowest_location_range)




