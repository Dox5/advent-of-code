import networkx
import doctest
import io
import typing
import enum
import itertools
import tqdm
import math

example_1 = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""

example_2 = """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

example_3 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""

# 0AL 2_R 4_L 5ZR 3_L
# 0   1   2   3   4   5   6
# 1AL 1AR 2_L 4_R 3_L 6ZR 6ZL

# offset = 3, 

my_example = """LR

0A = (2_, 2_)
1A = (1A, 2_)
2_ = (4_, 4_)
3_ = (6Z , 2_)
4_ = (5Z, 3_)
5Z = (7_ , 3_)
6Z = (1A, 6Z_)
"""

class Dir(enum.Flag):
    Left  = enum.auto()
    Right = enum.auto()

def read_map(fh: typing.TextIO) -> tuple[typing.Iterable[Dir], networkx.DiGraph]:
    instructions = fh.readline().strip()
    assert len(instructions) != 0
    assert set(instructions) == set(["L", "R"])

    # Expect a blank line
    assert fh.readline().strip() == ""

    # Now read the graph
    def iter_edges():
        for node in fh:
            name, left_and_right = [e.strip() for e in node.split("=")]
            left, right = [e.strip("() ") for e in left_and_right.split(",")]

            if left == right:
                # Special case where both directions go to the same place
                yield (name, left, {"d":Dir.Left | Dir.Right})
            else:
                yield (name, left, {"d":Dir.Left})
                yield (name, right, {"d":Dir.Right})

    desert_map = networkx.DiGraph()
    desert_map.add_edges_from(iter_edges())

    def make_inst():
        return [
            (Dir.Left if c == "L" else Dir.Right) for c in instructions
        ]


    return make_inst(), desert_map


def take_step(m: networkx.DiGraph, n: str, d: Dir) -> str:
        succ = m[n]

        # Make sure we've constructed the graph properly...
        assert len(succ) in range(1, 3)

        for next_node, edge_label in succ.items():
            if d in edge_label["d"]:
                return next_node

        # Only executed if we DIDN'T find a next label
        raise RuntimeError("The graph is broken!")


def total_steps(fh: typing.TextIO) -> int:
    """
    >>> total_steps(io.StringIO(example_1))
    2

    >>> total_steps(io.StringIO(example_2))
    6
    """
    inst, m = read_map(fh)

    steps_taken = 0
    current_node = "AAA"

    # THIS IS AN INFINITE LOOP DUE TO CYCLE NEED TO BREAK OUT OF IT
    for take_dir in itertools.cycle(inst):
        if current_node == "ZZZ":
            break

        # Taking another step fo sho
        steps_taken += 1

        current_node = take_step(m, current_node, take_dir)

    return steps_taken

def path_loop(path, looped_to, end_nodes):
    """
    >>> path_loop("ABCDEFG", "C", set("F"))
    (5, 5)
    >>> path_loop("ABCDEF", "C", set("F"))
    (5, 4)
    >>> path_loop("ABCDEFGH", "C", set("F"))
    (5, 6)

    >>> path_loop("ABCDEFGH", "E", set("F"))
    (5, 4)
    """
    # Split into 'intro' and cycle
    loop_start = path.index((looped_to))
    intro, cycle = path[0:loop_start], path[loop_start:]

    assert len(intro) + len(cycle) == len(path)

    assert [n for n in intro if n[0] in end_nodes] == []
    ends_in_cycle = [n for n in cycle if n[0] in end_nodes]

    steps_to_end = path.index(ends_in_cycle[0])

    if len(ends_in_cycle) == 1:
        # Seems to be easy - no ends in intro and a single end cycle!
        # Workout the offset to get to the end on the first iteration
        return steps_to_end, len(cycle)

    else:
        print("Only in example")
        # Only for the example -_-
        indexes = [cycle.index(e) for e in ends_in_cycle]
        diffs = [ i1 - i0 for i0, i1 in zip(indexes, indexes[1:]) ]
        assert all([d == diffs[0] for d in diffs])
        # This cycle repeats ends at a lower frequency than the full cycle so
        # can treat it at that lower frequency
        return steps_to_end, diffs[0]

def find_cycle(m, n, instructions):
    seen = set()

    end_nodes = set(n for n in m.nodes if n.endswith("Z"))
    current_node = n

    path = []
    
    for pos_in_seq, i in itertools.cycle(enumerate(instructions)):
        if (current_node, pos_in_seq) in seen:
            # Been here before so we found a loop
            break

        # Not at the end yet, take another step
        seen.add((current_node, pos_in_seq))
        path.append((current_node, pos_in_seq))
        current_node = take_step(m, current_node, i)

    # Now we have a loop generate check if there was exact 1 end found (which is
    # easier!

    return path_loop(path, (current_node, pos_in_seq), end_nodes)




def on_end(c, steps_taken):
    """
    >>> on_end((3, 8), 2)
    False
    >>> on_end((3, 8), 3)
    True
    >>> on_end((3, 8), 4)
    False
    >>> on_end((3, 8), 10)
    False
    >>> on_end((3, 8), 11)
    True
    >>> on_end((3, 8), 12)
    False
    """
    offset, cycle_len = c
    if steps_taken < offset:
        return False
    else:
        return ((steps_taken - offset) % cycle_len) == 0

def jump_to_next(c, steps_taken):
    """
    >>> jump_to_next((3, 8), 1)
    2
    >>> jump_to_next((3, 8), 2)
    1
    >>> jump_to_next((3, 8), 3)
    8
    >>> jump_to_next((3, 8), 4)
    7
    >>> jump_to_next((3, 8), 10)
    1
    >>> jump_to_next((3, 8), 11)
    8
    >>> jump_to_next((3, 8), 12)
    7
    """
    offset, cycle_len = c
    if steps_taken < offset:
        return offset - steps_taken 
    else:
        return cycle_len - ((steps_taken - offset) % cycle_len)



def total_ghost_steps(fh: typing.TextIO) -> int:
    """
    >>> total_ghost_steps(io.StringIO(example_3))
    6

    >>> total_ghost_steps(io.StringIO(my_example))
    6

    """
    inst, m = read_map(fh)


    cycles = sorted([find_cycle(m, n, inst) for n in m.nodes if n.endswith("A")], key=lambda x: x[0])


    step = 0

    print("cycle period:", math.prod(c[1] for c in cycles))

    aligned = cycles[0]


    with tqdm.tqdm() as pbar:
        for c in cycles[1:]:
            tried = 0
            while True:
                if all([on_end(c, step), on_end(aligned, step)]):
                    # Got alignment!
                    _, a_clen = aligned
                    _, c_clen = c
                    print("Found alignment between", aligned, c, "after", tried)
                    aligned = (step, math.lcm(a_clen, c_clen))
                    print("  Alignment", aligned)
                    break
                
                tried += 1

                # Where can we possibly match next
                jump = max([jump_to_next(c, step), jump_to_next(aligned, step)])
                step += jump 
                pbar.update(jump)

    print([on_end(c, step) for c in cycles])
    assert all(on_end(c, step) for c in cycles)

    return step


    return steps_taken

if __name__ == "__main__":
    doctest.testmod()

    with open("input.txt") as fh:
        print("Total steps", total_steps(fh))

    with open("input.txt") as fh:
        print("Total ghost steps", total_ghost_steps(fh))

