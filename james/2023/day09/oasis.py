import doctest
import io
import collections
import typing
from pprint import pprint


examples = [

"0 3 6 9 12 15",
"1 3 6 10 15 21",
"10 13 16 21 30 45",

]

def to_int_list(line: str) -> list[int]:
    """
    >>> to_int_list("1 3 5")
    [1, 3, 5]
    """
    return [int(x) for x in line.strip().split()]


# Look at this broken mess from trying to be too clever! It wasn't even needed
def next_in_seq(seq: list[int]) -> int:
    """
    >>> next_in_seq([0, 3, 6, 9, 12, 15])
    18
    """
    # list of lists, first level is the orginal sequence
    # if a value is missing it needs to be calculated (eg it may be NONE)
    #
    levels = [
        seq
    ]

    q = collections.deque()
    # Go work out what goes at this position
    q.append((0, len(seq)))

    zero_level = None

    def retrieve_or_queue(request: list[tuple[int, int]]) -> typing.Optional[list[int]]:
        retrieved = []
        for l, i in request:
            if l == 0 and i < len(levels[0]):
                # This is in the original input level, always available
                retrieved.append(levels[0][i])
            elif zero_level is not None and l >= zero_level:
                # If the zero layer is set then anything at that layer is just 0
                retrieved.append(0)
            elif l >= len(levels):
                # For a level not yet touched
                retrieved.append(None)
            elif i>= len(levels[l]):
                # Past end of level
                retrieved.append(None)
            else:
                retrieved.append(levels[l][i])



        at_least_one_none = False

        for request, v in zip(request, retrieved):
            if v is None:
                # This one needs to be handled
                at_least_one_none = True
                #print("q+", request)
                q.append(request)

        if at_least_one_none:
            # Not all requisites found
            return None
        else:
            return retrieved

    def set_level_value(level, i, v):

        assert i <= len(levels[level])

        if len(levels[level]) == i:
            levels[level].append(v)
        elif levels[level][i] is not None:
            assert levels[level][i] == v
            #print("Duplicated work!")
        else:
            levels[level][i] = v




    while True:
        try:
            level, requested = q.popleft()
        except IndexError:
            break

        #print("---")
        #print(level, requested)
        
        if zero_level is not None and level >= zero_level:
            # Skip anything queued before we knew where zero was
            continue
        
        try:
            # We already have this value so don't do anything
            if levels[level][requested] != None:
                continue
        except IndexError:
            pass

        # level is an index so need to add +1 for that and range is exclusive of
        # the last value
        for level_len in range(len(levels), level+1):
            levels.append([None] * (len(seq) - level_len))

        #print("levels", len(levels), [l for l in levels])


        new_value = requested >= len(levels[level])

        if new_value:
            # Have we found 2 zeros in this layer?
            # always calculated in the same way
            # Make space for the new value at this layer!
            #print("nv",len(levels[level]), requested)
            found = retrieve_or_queue([(level, requested-1), (level+1, requested-1)])
            if found is None:
                # At least one prereq not satisfied - go around again
                #print("q+", (level, requested))
                q.append((level, requested))
            else:
                prev, adjust = found
                # Generate the new value!
                set_level_value(level, requested, prev + adjust)
        else:
            #print("diff")
            # Not a 'new' value - generated by diffing existing values
            found = retrieve_or_queue([(level-1, requested+1), (level-1, requested)])

            if found is None:
                # At least one pre-req missing
                q.append((level, requested))
            else:
                #print("got pre-reqs")
                # Generate by diff
                higher, lower = found
                if zero_level is None and higher - lower == 0:
                    #print("Found zero at", level)
                    zero_level = level
                #print("setting value")
                set_level_value(level, requested, higher - lower)

    return levels[0][-1]

def make_next_level(level):

    prev = None
    for v in level:
        if prev is not None:
            yield v - prev
        prev = v

def do_dumb(level: list[int], forwards=True) -> int:
    """
    >>> do_dumb([0, 3, 6, 9, 12, 15])
    18
    >>> do_dumb([0, 3, 6, 9, 12, 15], forwards=False)
    -3

    >>> do_dumb([1, 3, 6, 10, 15, 21])
    28
    >>> do_dumb([1, 3, 6, 10, 15, 21], forwards=False)
    0

    >>> do_dumb([10, 13, 16, 21, 30, 45])
    68
    >>> do_dumb([10, 13, 16, 21, 30, 45], forwards=False)
    5
    """
    levels = [list(make_next_level(level))]

    while True:
        if all(l == 0 for l in levels[-1]):
            break;
        levels.append(list(make_next_level(levels[-1])))

    if forwards:
        return level[-1] + sum(l[-1] for l in levels)
    else:

        firsts = [level[0]] + [l[0] for l in levels]

        new_firsts = []

        prev = 0
        for f in reversed(firsts):
            # Walk backwards
            x = f - prev
            new_firsts.append(x)
            prev = x

        return new_firsts[-1]




if __name__ == "__main__":
    next_in_seq([0, 3, 6, 9, 12, 15])
    doctest.testmod()

    with open("input.txt") as fh:
        print("sum", sum(do_dumb(to_int_list(l)) for l in fh))

    with open("input.txt") as fh:
        print("sum (backward)", sum(do_dumb(to_int_list(l), forwards=False) for l in fh))