from typing import NamedTuple
import pathlib
import math
import pprint

class Game(NamedTuple):
    id: int
    samples: list[dict[str, int]]

def parse_game(line) -> Game:
    game_id_str, samples_line = line.split(":", 1)

    samples = []

    for sample in samples_line.split(";"):
        picked = {}

        for num_col in sample.split(","):
            num_col = num_col.strip()
            num, col = num_col.split(" ")
            picked[col] = int(num)

        samples.append(picked)

    _, id_str = game_id_str.split(" ", 1)

    return Game(id=int(id_str), samples=samples)

def game_passes(constraints: dict[str, int], g: Game) -> bool:
    for sample in g.samples:
        for colour, count in sample.items():
            if count > constraints[colour]:
                return False

    return True

def min_cubes_required(g: Game) -> dict[str, int]:
    mins = dict()

    for sample in g.samples:
        for colour, count in sample.items():
            if mins.get(colour, 0) < count:
                mins[colour] = count

    return mins

def part_a(games):
    constraints = {
        "red": 12,
        "green": 13,
        "blue": 14,
    }

    part_a_games = [g for g in games if game_passes(constraints, g)]

    answer = sum(g.id for g in part_a_games)
    print("Part A sum of IDs:", answer)

def power_set(d: dict[str, int]) -> int:
    return math.prod(d.values())

def part_b(games):
    mins = [min_cubes_required(g) for g in games]

    print("Sum of powersets", sum(power_set(m) for m in mins))

def main():


    games = []
    
    with pathlib.Path("input.txt").open("r") as f:
        for line in f:
            games.append(parse_game(line))

    part_a(games)
    part_b(games)






if __name__ == "__main__":
    main()
