use adventofcode::util::{get_world_extent, Point};
use std::collections::HashSet;
use std::io;

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
struct Antenna {
    freq: char,
    loc: Point,
}

fn parse_antenna_locations<R>(input: R) -> Vec<Antenna>
where
    R: io::BufRead,
{
    let mut antennas: Vec<Antenna> = input
        .lines()
        .map(io::Result::unwrap)
        .zip(0i64..)
        .flat_map(|(l, y)| {
            l.chars()
                .zip(0i64..)
                .filter_map(move |(f, x)| {
                    if f != '.' {
                        Some(Antenna {
                            freq: f,
                            loc: Point { x, y },
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect();

    // Sort so all antennas of a given type are togther
    antennas.sort_unstable_by_key(|a| a.freq);
    return antennas;
}

fn antinodes(antennas: &Vec<Antenna>, world_extent: Point) -> Vec<Antenna> {
    // antenna must be sorted by 'freq' already
    let mut antinodes = Vec::<Antenna>::new();

    for i in 0..antennas.len() {
        let ant = antennas[i];

        antennas[i + 1..]
            .iter()
            .take_while(|a| a.freq == ant.freq)
            .for_each(|a| {
                let vector = ant.loc - a.loc;

                let anti_a = ant.loc + vector;
                if anti_a.within(Point { x: 0, y: 0 }, world_extent) {
                    antinodes.push(Antenna {
                        freq: a.freq,
                        loc: anti_a,
                    });
                }

                let anti_b = a.loc + vector.reverse();
                if anti_b.within(Point { x: 0, y: 0 }, world_extent) {
                    antinodes.push(Antenna {
                        freq: a.freq,
                        loc: anti_b,
                    });
                }
            });
    }

    return antinodes;
}

fn antinodes_harmonics(antennas: &Vec<Antenna>, world_extent: Point) -> Vec<Antenna> {
    // antenna must be sorted by 'freq' already
    let mut antinodes = Vec::<Antenna>::new();

    for i in 0..antennas.len() {
        let ant = antennas[i];

        antennas[i + 1..]
            .iter()
            .take_while(|a| a.freq == ant.freq)
            .for_each(|a| {
                let vector = ant.loc - a.loc;

                let mut anti_a = ant.loc;
                while anti_a.within(Point { x: 0, y: 0 }, world_extent) {
                    antinodes.push(Antenna {
                        freq: a.freq,
                        loc: anti_a,
                    });
                    anti_a = anti_a + vector;
                }

                let mut anti_b = a.loc;
                while anti_b.within(Point { x: 0, y: 0 }, world_extent) {
                    antinodes.push(Antenna {
                        freq: a.freq,
                        loc: anti_b,
                    });
                    anti_b = anti_b + vector.reverse();
                }
            });
    }

    return antinodes;
}

fn main() {
    let antennas = parse_antenna_locations(io::BufReader::new(
        std::fs::File::open("inputs/day08.txt").expect("failed to read input"),
    ));
    let world_extent = get_world_extent(io::BufReader::new(
        std::fs::File::open("inputs/day08.txt").expect("failed to read input"),
    ));

    let unique: HashSet<Point> = antinodes(&antennas, world_extent)
        .into_iter()
        .map(|a| a.loc)
        .collect();
    println!("Part1, unique_antinodes: {}", unique.len());

    let unique_harmonics: HashSet<Point> = antinodes_harmonics(&antennas, world_extent)
        .into_iter()
        .map(|a| a.loc)
        .collect();
    println!(
        "Part2, unique_antinodes with harmonics: {}",
        unique_harmonics.len()
    );
}

#[cfg(test)]
mod day08_tests {
    use super::*;
    use rstest::rstest;

    const EXAMPLE_INPUT: &'static str = "\
    ............\n\
    ........0...\n\
    .....0......\n\
    .......0....\n\
    ....0.......\n\
    ......A.....\n\
    ............\n\
    ............\n\
    ........A...\n\
    .........A..\n\
    ............\n\
    ............\n\
    ";

    fn antenna(freq: char, x: i64, y: i64) -> Antenna {
        return Antenna {
            freq,
            loc: Point { x, y },
        };
    }

    #[rstest]
    fn test_simple_antinodes() {
        let expected = vec![antenna('a', 3, 1), antenna('a', 6, 7)];
        let input = vec![antenna('a', 4, 3), antenna('a', 5, 5)];

        assert_eq!(expected, antinodes(&input, Point { x: 8, y: 8 }));
    }

    #[rstest]
    fn test_example_part_1() {
        let reader = io::BufReader::new(EXAMPLE_INPUT.as_bytes());
        let antennas = parse_antenna_locations(reader);
        let world_extent = get_world_extent(EXAMPLE_INPUT.as_bytes());

        println!("world extend {world_extent}");

        let unique: HashSet<Point> = antinodes(&antennas, world_extent)
            .into_iter()
            .map(|a| a.loc)
            .collect();

        assert_eq!(14, unique.len());
    }

    #[rstest]
    fn test_example_part_2() {
        let reader = io::BufReader::new(EXAMPLE_INPUT.as_bytes());
        let antennas = parse_antenna_locations(reader);
        let world_extent = get_world_extent(EXAMPLE_INPUT.as_bytes());

        println!("world extend {world_extent}");

        let unique: HashSet<Point> = antinodes_harmonics(&antennas, world_extent)
            .into_iter()
            .map(|a| a.loc)
            .collect();

        assert_eq!(34, unique.len());
    }

    #[rstest]
    #[case(".A..\n...B", vec![antenna('A', 1, 0), antenna('B', 3, 1)])]
    fn test_parse_loc(#[case] input: &str, #[case] expected: Vec<Antenna>) {
        let reader = io::BufReader::new(input.as_bytes());
        let actual = parse_antenna_locations(reader);

        assert_eq!(expected, actual);
    }
}
