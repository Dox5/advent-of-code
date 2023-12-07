import doctest
import typing
import enum
import io
import pprint
import collections

example_input="""32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

ten_and_up = {
    "T": 10,
    "J": 11,
    "Q": 12,
    "K": 13,
    "A": 14
}

def sort_order_of(face_value: str, special_rule) -> int:
    codepoint = ord(face_value)
    if codepoint in range(50, 58):
        # ASCII text
        return codepoint - 48
    elif face_value in ten_and_up:
        # Special rule maps J to lowest value
        if special_rule and face_value == "J":
            return 1
        else:
            return ten_and_up[face_value]
    else:
        raise RuntimeError(f"What is {face_value}")

wtf = set()
class Card:
    """
    >>> Card("A") > Card("7")
    True
    >>> Card("5") < Card("T")
    True
    >>> Card("A") > Card("T")
    True
    >>> Card("J", True) < Card("2", True)
    True
    >>> Card("J", True) < Card("A", True)
    True
    """
    def __init__(self, face_value: str, special_rule: bool = False):
        self._face_value = face_value
        self._sort_order = sort_order_of(face_value, special_rule)
        if special_rule:
            wtf.add((self._face_value, self._sort_order))

    @property
    def value(self):
        return self._face_value

    def __hash__(self):
        return hash(self._sort_order)

    def __eq__(self, other):
        return self._sort_order == other._sort_order

    def __lt__(self, other):
        return self._sort_order < other._sort_order

    def __repr__(self) -> str:
        return f"Card('{self.value}')"

    def __str__(self) -> str:
        return f"{self.value}"
        


class Combo(enum.IntEnum):
    """
    >>> Combo.HighCard < Combo.OnePair
    True

    >>> Combo.ThreeKind > Combo.TwoPair
    True
    """
    HighCard  = enum.auto()
    OnePair   = enum.auto()
    TwoPair   = enum.auto()
    ThreeKind = enum.auto()
    FullHouse = enum.auto()
    FourKind  = enum.auto()
    FiveKind  = enum.auto()


possible_hands_num_cards = {
    0: set(),
    1: set([Combo.HighCard]),
    2: set([Combo.HighCard, Combo.OnePair]), 
    3: set([Combo.HighCard, Combo.OnePair, Combo.ThreeKind]), 
    4: set([Combo.HighCard, Combo.OnePair, Combo.ThreeKind, Combo.TwoPair, Combo.FourKind]), 
    5: set([Combo.HighCard, Combo.OnePair, Combo.ThreeKind, Combo.TwoPair, Combo.FourKind, Combo.FullHouse, Combo.FiveKind]), 
}

possible_hands_card_sets = {
    0: set(),
    1: set([Combo.HighCard, Combo.OnePair, Combo.ThreeKind, Combo.FourKind, Combo.FiveKind]),
    2: set([Combo.HighCard, Combo.OnePair, Combo.TwoPair, Combo.ThreeKind, Combo.FullHouse, Combo.FourKind]), 
    3: set([Combo.HighCard, Combo.OnePair, Combo.TwoPair, Combo.ThreeKind]), 
    4: set([Combo.HighCard, Combo.OnePair]), 
    5: set([Combo.HighCard]), 
}

# num cards, num sets
phands = {
    (1, 1): set([Combo.HighCard]),

    (2, 1): set([Combo.OnePair]),
    (2, 2): set([Combo.HighCard]),

    (3, 1): set([Combo.ThreeKind]),
    (3, 2): set([Combo.OnePair]),
    (3, 3): set([Combo.HighCard]),

    (4, 1): set([Combo.FourKind]),
    (4, 2): set([Combo.TwoPair, Combo.ThreeKind]),
    (4, 3): set([Combo.OnePair]),
    (4, 4): set([Combo.HighCard]),

    (5, 1): set([Combo.FiveKind]),
    (5, 2): set([Combo.FullHouse, Combo.FourKind]),
    (5, 3): set([Combo.TwoPair, Combo.ThreeKind]),
    (5, 4): set([Combo.OnePair]),
    (5, 5): set([Combo.HighCard]),
}

def _combo_from_cards_maybe_broken(cards: list[Card]) -> Combo:
    """
    >>> combo_from_cards("555A5")
    <Combo.FourKind: 6>
    >>> combo_from_cards("999BB")
    <Combo.FullHouse: 5>

    >>> combo_from_cards("AABB")
    <Combo.TwoPair: 3>
    >>> combo_from_cards("AAAB")
    <Combo.ThreeKind: 4>
    """
    card_counts = dict()

    for c in cards:
        count = card_counts.get(c, 0)
        card_counts[c] = count + 1

    counts = list(card_counts.values())
    sets = len(counts)

    possible_hands = phands[(len(cards), sets)]

    if len(possible_hands) == 1:
    #    print("Only 1 possible")
        return list(possible_hands)[0]
    
    counts.sort()
    largest_set = counts[-1]

    # Need to disambiguate some
    if len(cards) == 4:
        if largest_set == 3:
            return Combo.ThreeKind
        elif largest_set == 2:
            return Combo.TwoPair

    elif len(cards) == 5:
        if sets == 2:
            if largest_set == 4:
                return Combo.FourKind
            elif largest_set == 3:
                return Combo.FullHouse
        elif sets == 3:
            if largest_set == 3:
                return Combo.ThreeKind
            elif largest_set == 2:
                return Combo.TwoPair

    breakpoint()


    raise RuntimeError("eek")

def _combo_from_cards(cards: list[Card]) -> Combo:
    """
    >>> combo_from_cards("555A5")
    <Combo.FourKind: 6>
    >>> combo_from_cards("999BB")
    <Combo.FullHouse: 5>

    >>> combo_from_cards("AABB")
    <Combo.TwoPair: 3>
    >>> combo_from_cards("AAAB")
    <Combo.ThreeKind: 4>
    """
    counts = collections.Counter(cards)
    most_common = [c[1] for c in counts.most_common()]

    if most_common[0] == 5:
        return Combo.FiveKind
    elif most_common[0] == 4:
        return Combo.FourKind
    elif most_common[0] == 3:
        if len(most_common) == 1:
            return Combo.ThreeKind
        elif most_common[1] == 2:
            return Combo.FullHouse
        elif most_common[1] == 1:
            return Combo.ThreeKind
    elif most_common[0] == 2:
        if len(most_common) == 1:
            return Combo.OnePair
        elif most_common[1] == 2:
            return Combo.TwoPair
        elif most_common[1] == 1:
            return Combo.OnePair
    elif most_common[0] == 1:
        return Combo.HighCard


def to_card_list(cards : str, special_rule=False) -> list[Card]:
    return [Card(c, special_rule) for c in cards]


def combo_from_cards(cards: list[Card], special_rule=False) -> Combo:
    """
    >>> combo_from_cards(to_card_list("A", True), True)
    <Combo.HighCard: 1>
    >>> combo_from_cards(to_card_list("A2", True), True)
    <Combo.HighCard: 1>
    >>> combo_from_cards(to_card_list("A23", True), True)
    <Combo.HighCard: 1>
    >>> combo_from_cards(to_card_list("A234", True), True)
    <Combo.HighCard: 1>
    >>> combo_from_cards(to_card_list("A2345", True), True)
    <Combo.HighCard: 1>
    >>> combo_from_cards(to_card_list("JA", True), True)
    <Combo.OnePair: 2>
    >>> combo_from_cards(to_card_list("JA2", True), True)
    <Combo.OnePair: 2>
    >>> combo_from_cards(to_card_list("JA23", True), True)
    <Combo.OnePair: 2>
    >>> combo_from_cards(to_card_list("JA234", True), True)
    <Combo.OnePair: 2>
    >>> combo_from_cards(to_card_list("JJA", True), True)
    <Combo.ThreeKind: 4>
    >>> combo_from_cards(to_card_list("JJJA", True), True)
    <Combo.FourKind: 6>
    >>> combo_from_cards(to_card_list("JJJJA", True), True)
    <Combo.FiveKind: 7>

    >>> combo_from_cards(to_card_list("AA", True), True)
    <Combo.OnePair: 2>
    >>> combo_from_cards(to_card_list("AA2", True), True)
    <Combo.OnePair: 2>
    >>> combo_from_cards(to_card_list("AA23", True), True)
    <Combo.OnePair: 2>
    >>> combo_from_cards(to_card_list("AA234", True), True)
    <Combo.OnePair: 2>
    >>> combo_from_cards(to_card_list("JAA", True), True)
    <Combo.ThreeKind: 4>
    >>> combo_from_cards(to_card_list("JJAA", True), True)
    <Combo.FourKind: 6>
    >>> combo_from_cards(to_card_list("JJJAA", True), True)
    <Combo.FiveKind: 7>

    >>> combo_from_cards(to_card_list("QQAA", True), True)
    <Combo.TwoPair: 3>
    >>> combo_from_cards(to_card_list("QQAA2", True), True)
    <Combo.TwoPair: 3>
    >>> combo_from_cards(to_card_list("JQQAA", True), True)
    <Combo.FullHouse: 5>

    >>> combo_from_cards(to_card_list("AAA", True), True)
    <Combo.ThreeKind: 4>
    >>> combo_from_cards(to_card_list("AAA2", True), True)
    <Combo.ThreeKind: 4>
    >>> combo_from_cards(to_card_list("AAA23", True), True)
    <Combo.ThreeKind: 4>
    >>> combo_from_cards(to_card_list("JAAA", True), True)
    <Combo.FourKind: 6>
    >>> combo_from_cards(to_card_list("JJAAA", True), True)
    <Combo.FiveKind: 7>

    >>> combo_from_cards(to_card_list("AAA99", True), True)
    <Combo.FullHouse: 5>

    >>> combo_from_cards(to_card_list("AAAA2", True), True)
    <Combo.FourKind: 6>
    >>> combo_from_cards(to_card_list("JAAAA", True), True)
    <Combo.FiveKind: 7>

    >>> combo_from_cards(to_card_list("AAAAA", True), True)
    <Combo.FiveKind: 7>
    >>> combo_from_cards(to_card_list("JJJJJ", True), True)
    <Combo.FiveKind: 7>

    """
    if not special_rule:
        return _combo_from_cards(cards)

    jokers = []
    non_jokers = []
    for c in cards:
        if c.value == "J":
            jokers.append(c)
        else:
            non_jokers.append(c)

    assert len(jokers) + len(non_jokers) == len(cards)
    assert [j for j in jokers if j.value != "J"] == []
    assert [n for n in non_jokers if n.value == "J"] == []

    if len(non_jokers) == 0:
        assert [c for c in cards if c.value != "J"] == []
        return Combo.FiveKind

    best_combo = _combo_from_cards(non_jokers)

    # 4kind (1), 4kind (2), 4kind(1)
    # HighCard
    # OnePair   = enum.auto()
    # TwoPair   = enum.auto()
    # ThreeKind = enum.auto()
    # FullHouse = enum.auto()
    # FourKind  = enum.auto()
    # FiveKind  = enum.auto()

    return {
        (Combo.HighCard, 1): Combo.OnePair,
        (Combo.HighCard, 2): Combo.ThreeKind,
        (Combo.HighCard, 3): Combo.FourKind,
        (Combo.HighCard, 4): Combo.FiveKind,

        (Combo.OnePair, 1): Combo.ThreeKind,
        (Combo.OnePair, 2): Combo.FourKind,
        (Combo.OnePair, 3): Combo.FiveKind,

        (Combo.TwoPair, 1): Combo.FullHouse,

        (Combo.ThreeKind, 1): Combo.FourKind,
        (Combo.ThreeKind, 2): Combo.FiveKind,

        (Combo.FourKind, 1): Combo.FiveKind,

    }.get((best_combo, len(jokers)), best_combo)



    


class Hand(typing.NamedTuple):
    """
    >>> Hand(combo=Combo.ThreeKind, cards=to_card_list("JAA23", True)) < Hand(combo=Combo.ThreeKind, cards=to_card_list("AAJ23", True))
    True
    """
    # combo goes FIRST so that's the primary sort key. Deal with it!
    combo: Combo
    cards: list[Card]

def parse_hand(card_str: str, special_rule=False):
    """
    >>> parse_hand("AA234")
    Hand(combo=<Combo.OnePair: 2>, cards=[Card('A'), Card('A'), Card('2'), Card('3'), Card('4')])
    """
    cards = to_card_list(card_str, special_rule)

    return Hand(cards=cards, combo=combo_from_cards(cards, special_rule))

def read_games(fh, special_rule) -> typing.Iterable[tuple[Hand, int]]:
    for line in fh:
        line = line.strip()
        cards, score = line.split(" ")
        yield parse_hand(cards, special_rule), int(score)



def total_winnings(fh, special_rule=False) -> int:
    """
    >>> total_winnings(io.StringIO(example_input))
    6440
    >>> total_winnings(io.StringIO(example_input), True)
    5905
    """
    rounds = list(read_games(fh, special_rule))
    rounds.sort(key=lambda r: r[0])

    winnings = 0
    for i, (_, won) in enumerate(rounds):
        rank = i + 1
        winnings += won * rank

    return winnings





if __name__ == "__main__":
    doctest.testmod()

    with open("input.txt") as fh:
        print("Total winnings:", total_winnings(fh))

    with open("input.txt") as fh:
        print("Total winnings (special rule):", total_winnings(fh, True))
