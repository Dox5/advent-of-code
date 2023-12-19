import doctest
import typing
import collections
from dataclasses import dataclass, field


@dataclass
class Lens:
    """
    >>> Lens(label="abc", focal_length=1) == Lens(label="abc", focal_length=3)
    True

    >>> Lens(label="abc", focal_length=1) == Lens(label="def", focal_length=1)
    False
    """
    label: str
    focal_length: int = field(compare=False)

class Box:
    def __init__(self):
        self._lenses = collections.deque()

    def remove(self, lens):
        try:
            self._lenses.remove(lens)
        except ValueError:
            # Allow label to be missing
            pass

    @property
    def lenses(self):
        return list(self._lenses)

    def replace(self, lens: Lens):
        """
        >>> b = Box(); b.replace(Lens("a", 1)) ; b.replace(Lens("a", 7)); b.lenses
        [Lens(label='a', focal_length=7)]

        >>> b = Box(); b.replace(Lens("q", 6)); b.replace(Lens("a", 1)) ; b.replace(Lens("a", 7)); b.lenses
        [Lens(label='q', focal_length=6), Lens(label='a', focal_length=7)]
        """
        try:
            existing = self._lenses.index(lens)
            # Replace it
            del self._lenses[existing]
            self._lenses.insert(existing, lens)
        except ValueError:
            # Not present, just insert at the end
            self._lenses.append(lens)

    def focus_power(self) -> int:
        total = 0
        for slot, box in enumerate(self._lenses, start=1):
            total += slot * box.focal_length

        return total






def h_a_s_h(txt: typing.Iterable[str]) -> int:
    """
    >>> h_a_s_h("rn=1")
    30

    >>> h_a_s_h("pc=4")
    180

    """
    h = 0
    for c in txt:
        h += ord(c)
        h *= 17
        h = h % 256
    return h


example_1 = ["rn=1", "cm-", "qp=3", "cm=2", "qp-", "pc=4", "ot=9", "ab=5", "pc-", "pc=6", "ot=7"]

def verification_code(instructions: list[str]) -> int:
    """
    >>> verification_code(example_1)
    1320
    """
    return sum(h_a_s_h(i) for i in instructions)

def split_inst(i: str) -> tuple[Lens, str]:
    label = []
    op = ""
    focal = []

    for c in i:
        if op == "":
            if c == "=" or c == "-":
                op = c
            else:
                label += c
        else:
            focal += c

    if op == "=":
        return Lens(label="".join(label), focal_length=int("".join(focal))), op
    else:
        return Lens(label="".join(label), focal_length=-1), op



def arrange_lenses(instructions: list[str]) -> list[Box]:
    boxes = [Box() for _ in range(256)]

    for i in instructions:
        lens, op = split_inst(i)

        box_n = h_a_s_h(lens.label)

        if op == "=":
            boxes[box_n].replace(lens)
        elif op == "-":
            boxes[box_n].remove(lens)

    return boxes

def focus_power(boxes: list[Box]) -> int:
    total = 0

    for pos, box in enumerate(boxes, start=1):
        power = box.focus_power()
        total += pos * power

    return total


def part_b(instructions: list[str]) -> int:
    """
    >>> part_b(example_1)
    145
    """
    boxes = arrange_lenses(instructions)
    return focus_power(boxes)


if __name__ == "__main__":
    doctest.testmod()
    with open("input.txt") as fh:
        instructions = fh.readline().strip().split(",")
        print("Verification code:", verification_code(instructions))
        print("Focus power:", part_b(instructions))
