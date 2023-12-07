import typing
import io
import math
import bisect

example_input = """Time:      7  15   30
Distance:  9  40  200
"""

part_a_input = """Time:        44     89     96     91
Distance:   277   1136   1890   1768
"""

part_b_input = """Time:        44899691
Distance:   277113618901768
"""

def split_range(r: range) -> (range, range):
    """
    >>> split_range(range(3, 5))
    (range(3, 4), range(4, 5))

    >>> split_range(range(7, 10))
    (range(7, 8), range(8, 10))
    """
    assert len(r) > 1

    rough_half = int(len(r)/2)

    return ( range(r.start, r.start+rough_half),
             range(r.start+rough_half, r.stop))

    


class Race(typing.NamedTuple):
    time_ms: int
    dist_mm: int

    def winning_times(self) -> range:
        """
        >>> Race(time_ms=7, dist_mm=9).winning_times()
        range(2, 6)

        >>> Race(time_ms=15, dist_mm=40).winning_times()
        range(4, 12)

        >>> Race(time_ms=30, dist_mm=200).winning_times()
        range(11, 20)
        """

        press_times = range(1, self.time_ms)

        def calc_dist(press_time):
            velocity = press_time
            travel_time = self.time_ms - press_time
            return travel_time * velocity

        def pre_mid(press_time):
            # At the end
            if press_time+1 not in press_times:
                return False

            return calc_dist(press_time) <= calc_dist(press_time+1)

        mid_point = press_times
        while True:
            if len(mid_point) == 1:
                # Found it
                break

            lhs, rhs = split_range(mid_point)

            if pre_mid(lhs.stop-1):
                mid_point = rhs
            else:
                mid_point = lhs

        start_side = range(press_times.start, mid_point.stop)
        stop_side  = range(press_times.stop-1, mid_point.start, -1) 

        start_pos = bisect.bisect_left(start_side, self.dist_mm+1, key=calc_dist)
        stop_pos = bisect.bisect_left(stop_side, self.dist_mm+1, key=calc_dist)
        if stop_pos < len(stop_side):
            return range(start_side[start_pos], stop_side[stop_pos]+1)
        else:
            # Nothing after mid beats!
            return range(start_side[start_pos], start_side.stop)


def prefixed_num_list(l: str) -> list[int]:
    prefix, nums = l.split(":", 1)
    return [int(n) for n in nums.split()]


def read_races(fh: typing.TextIO) -> list[Race]:
    """
    >>> read_races(io.StringIO(example_input))
    [Race(time_ms=7, dist_mm=9), Race(time_ms=15, dist_mm=40), Race(time_ms=30, dist_mm=200)]
    """
    lines = fh.readlines()
    assert len(lines) == 2

    return [ Race(time_ms=t, dist_mm=d) for t, d in zip(prefixed_num_list(lines[0]), prefixed_num_list(lines[1]))]


def margin_of_error(races: list[Race]) -> int:
    """
    >>> margin_of_error(read_races(io.StringIO(example_input)))
    288
    """
    return math.prod(len(r.winning_times()) for r in races)


if __name__ == "__main__":
    import doctest
    doctest.testmod()

    print("Margin of error: ", margin_of_error(read_races(io.StringIO(part_a_input))))

    print("Margin of error partb: ", margin_of_error(read_races(io.StringIO(part_b_input))))

