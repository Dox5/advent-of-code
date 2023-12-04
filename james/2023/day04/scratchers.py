import typing
import pathlib
import io
import collections
import functools

example = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

class Card(typing.NamedTuple):
    id: int
    winning: set[int]
    have: set[int]

def num_list_to_set(num_list: str) -> set[int]:
    return { int(n) for n in num_list.split() }

def parse_card(line: str):
    """
    >>> parse_card("Card 1:  41 48 83 86 17 | 83 86  6 31 17  9 48 53\\n")
    Card(id=1, winning={41, 48, 83, 86, 17}, have={3, 86,  6, 31, 17, 9, 48, 53})
    """
    line = line.strip()
    card_id, rest = line.split(":")
    _, card_id = card_id.split(" ", 1)

    winning, have = rest.split("|", 1)

    return Card(id=int(card_id), winning=num_list_to_set(winning), have=num_list_to_set(have))

def iter_cards(stream) -> typing.Iterable[Card]:
    for line in stream:
        yield parse_card(line)


def part_a(stream):
    """
    >>> part_a(io.StringIO(example))
    13
    """

    score = 0

    for card in iter_cards(stream):
        wins = card.winning & card.have
        # Need at least 1 match to score at all
        if wins != set():
            score += 2**(len(wins)-1)

    return score

def part_b(stream):
    """
    >>> part_b(io.StringIO(example))
    30
    """
    cards = list(iter_cards(stream))

    inventory = { c.id: 0 for c in cards }

    for card in cards:
        # First account for this card in the inventory
        n_of_this = inventory[card.id] + 1
        inventory[card.id] = n_of_this

        new_cards = cards[card.id: card.id+len(card.winning & card.have)]

        # Update wins
        for c in new_cards:
            # Gain multiple of each card
            inventory[c.id] += n_of_this
        

    return sum(inventory.values())


if __name__ == "__main__":
    import doctest
    doctest.testmod()

    with pathlib.Path("input.txt").open("r") as f:
        print("Point score total:", part_a(f))

    with pathlib.Path("input.txt").open("r") as f:
        print("Real game total cards:", part_b(f))
