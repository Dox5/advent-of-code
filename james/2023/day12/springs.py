import doctest
import functools

def parse_line(l: str) -> tuple[str, str]:
    springs, wants = l.split(" ", 1)
    return springs, tuple([int(w) for w in wants.split(",")])


calls = 0
@functools.cache
def r(springs: str, wants: tuple[int], remaining_want=None, depth=0):
    """
    >>> r("???.###", [1, 1, 3])
    1

    >>> r(".??..??...?##.", [1, 1, 3])
    4

    >>> r("?#?#?#?#?#?#?#?", [1, 3, 1, 6])
    1

    >>> r("????.#...#...", [4, 1, 1])
    1

    >>> r("????.######..#####.", [1, 6, 5])
    4

    >>> r("?###????????", [3, 2, 1])
    10

    >>> r("?".join([".??..??...?##."]*5), [1, 1, 3]*5)
    16384
    """
    global calls
    calls += 1
    if wants == ():
        # Run out of groups (all satisfied) make sure that we have no extra
        # groups
        try:
            springs.index("#")
            return 0
        except ValueError:
            # No more #, so we can set everything to .
            return 1

    # See if we can trim this branch, are there enough # + ? left to fulfil the
    # remaining groups
    if remaining_want is None:
        remaining_want = sum(wants)

    if springs.count("?") + springs.count("#") < remaining_want:
        # No point exploring this path further - it's impossible to finish any
        # groups
        return 0


    #print(" "*depth + "This layer", springs, wants[0])

    total = 0

    for i  in range(0, len(springs)):
        # walk forward from this point and try a build the first group we need
        have_springs = 0
        j = i

        while j < len(springs)+1:
            if j < len(springs):
                c = springs[j]
            else:
                # Past the end so pretend we have an extra group end to grab
                # those last groups!
                c = "."


            if c == "#":
                # Has to be a spring
                have_springs += 1

            elif have_springs == wants[0]:
                # Group is satisfied! try the next one, that might also be a bit
                # of this group
                total += r(springs[j+1:], wants[1:], remaining_want - wants[0], depth=depth+1)
                # Done on this branch
                break

            elif c == ".":
                # Hit a group end but haven't made a group of the right size.
                # We're done at this offset!
                break

            else:
                # Treat this as a # and see if we can build a group later
                have_springs += 1

            j += 1

        if springs[i] == "#":
            # Give up - can't walk the start past a #
            break


    return total


if __name__ == "__main__":
    doctest.testmod()

    with open("input.txt") as fh:
        total = 0
        for l in fh:
            state, wants = parse_line(l.strip())
            total += r(state, wants)

        print("Total:", total)

    with open("input.txt") as fh:
        total = 0
        for l in fh:
            state, wants = parse_line(l.strip())
            state = "?".join([state]*5)
            wants = wants*5
            print(state, wants, end=": ")
            calls = 0
            perms = r(state, wants)
            print(perms, f"({calls} calls)")
            total += perms

        print("Total unfolded:", total)
