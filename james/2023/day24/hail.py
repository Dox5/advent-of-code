import io
import math

from pprint import pprint
import doctest

from sympy import symbols, solve

example = """19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"""

X=0
Y=1
Z=2

def iter_stones(fh):
    for l in fh:
        l = l.strip()

        pos, vel = l.split("@")

        pos, vel = tuple(int(p.strip()) for p in pos.split(",")), tuple(int(p.strip()) for p in vel.split(","))

        yield pos, vel

def find_intersect(iseg, jseg):
    (x1, y1, _), (x2, y2, _) = iseg
    (x3, y3, _), (x4, y4, _) = jseg


    denom = (x1 - x2)*(y3-y4) - (y1-y2)*(x3-x4)
    if denom == 0:
        return None

    t = ( (x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)) / denom
    u = ( (x1-x3)*(y1-y2) - (y1 - y3)*(x1 - x2)) / denom

    if 0 <= t <= 1 and 0 <= u <= 1:
        return (x1 + t*(x2-x1), y1 + t*(y2-y1))
    else:
        return None


def extend(p, vel, bounds):
    """
    >>> extend(5, -1, range(0, 8))
    5
    >>> extend(5, 1, range(0, 8))
    2
    """
    if vel < 0:
        # Heading toward the lower bound
        remaining = p - bounds.start
        steps = remaining / abs(vel)
    else:
        # Heading toward the upper bound
        remaining = bounds.stop - p - 1
        steps = remaining / vel

    return steps


def find_end(p, v, bounds):
    x_steps = extend(p[X], v[X], bounds)
    y_steps = extend(p[Y], v[Y], bounds)

    steps = min(x_steps, y_steps)

    return p[X] + v[X]*steps, p[Y] + v[Y]*steps, p[Z] + v[Z]*steps







def part_a(stones, bounds):
    intersections = 0
    for i in range(len(stones)):
        ipos, ivel = stones[i]
        iseg = (ipos, find_end(ipos, ivel, bounds))

        for j in range(i+1, len(stones)):
            jpos, jvel = stones[j]
            jseg = (jpos, find_end(jpos, jvel, bounds))


            intersect = find_intersect(iseg, jseg)
            if intersect is not None:
                # THIS MEANS THE INTERSECTION CODE DOESN'T QUITE WORK CORRECTLY
                # It's because it doesn't clip the start of a line (eg one that
                # starts outside the bounding box!)
                if bounds.start <= intersect[X] < bounds.stop and bounds.start <= intersect[Y] < bounds.stop:
                    intersections += 1
                else:
                    print("Intersection between", ipos, jpos, "was out of bounds", intersect)


    return intersections

def part_b(stones):

    # Only need 3 stones to solve
    stones = stones[:3]

    # Create our single variables
    px, py, pz = symbols("px, py, pz")
    rx, ry, rz = symbols("rx, ry, rz")

    # Create time variables for each of our lines (when they intersect with R)
    th = symbols(f"th:{len(stones)}")

    system = []


    for i, ((hx, hy, hz), (vx, vy, vz)) in enumerate(stones):
        system.append(hx + vx*th[i] - px + rx*th[i])
        system.append(hy + vy*th[i] - py + ry*th[i])
        system.append(hz + vz*th[i] - pz + rz*th[i])


    sol = solve(system)[0]
    return sol[px], sol[py], sol[pz]

    

    



if __name__ == "__main__":
    doctest.testmod(verbose=True)
    example_stones = list(iter_stones(io.StringIO(example)))
    print("(example) Intersections in area", part_a(example_stones, range(7, 28)))
    print("(example) Throw point", part_b(example_stones))

    with open("input.txt") as fh:
        stones = list(iter_stones(fh))

    print("Intersections in area", part_a(stones, range(200000000000000, 400000000000001)))

    print("Throw point", part_b(stones))
