use itertools::Itertools;
use std::str::FromStr;

const XMAS: [u8; 4] = [0x58u8, 0x4Du8, 0x41u8, 0x53u8];
const SAMX: [u8; 4] = [0x53u8, 0x41u8, 0x4Du8, 0x58u8];

struct Grid {
    dim: usize,
    data: Vec<u8>,
}

#[derive(Debug, PartialEq, Eq)]
struct GridParseError;

impl FromStr for Grid {
    type Err = GridParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let width = s.chars().take_while(|&c| c != '\n').count();
        let height = s.chars().filter(|&c| c == '\n').count();

        if width != height {
            return Err(GridParseError {});
        }

        let data: Vec<u8> = s.bytes().filter(|&b| b != 0x0A).collect();

        return Ok(Grid { dim: width, data });
    }
}

impl Grid {
    fn count_horrizontal(&self) -> usize {
        let mut total = 0;
        // Horrizontal
        for row_iter in &self.data.iter().chunks(self.dim) {
            let row: Vec<u8> = row_iter.map(|&c| c).collect();
            total += count_words(&row);
        }

        return total;
    }

    fn count_vertical(&self) -> usize {
        let mut total = 0;

        for x in 0usize..self.dim {
            let col: Vec<u8> = self
                .data
                .iter()
                .skip(x)
                .step_by(self.dim)
                .map(|&c| c)
                .collect();
            total += count_words(&col);
        }

        return total;
    }

    fn count_backslash(&self) -> usize {
        let mut total = 0;

        for x in 0usize..self.dim {
            let limit = self.dim - x;
            let slash: Vec<u8> = self
                .data
                .iter()
                .skip(x)
                .step_by(self.dim + 1)
                .take(limit)
                .map(|&c| c)
                .collect();
            total += count_words(&slash);
        }

        // y = x already done in loop above
        for y in 1usize..self.dim {
            let limit = self.dim - y;
            let slash: Vec<u8> = self
                .data
                .iter()
                .skip(y * self.dim)
                .step_by(self.dim + 1)
                .take(limit)
                .map(|&c| c)
                .collect();
            total += count_words(&slash);
        }

        return total;
    }

    fn count_forwardslash(&self) -> usize {
        let mut total = 0;
        // 'forward slash'
        for x in 0usize..self.dim {
            let slash: Vec<u8> = self
                .data
                .iter()
                .skip(x)
                .step_by(self.dim - 1)
                .take(x + 1)
                .map(|&c| c)
                .collect();
            total += count_words(&slash);
        }

        // y = x already done in loop above
        for y in 1usize..self.dim {
            let slash: Vec<u8> = self
                .data
                .iter()
                .skip(y * self.dim)
                .skip(self.dim - 1)
                .step_by(self.dim - 1)
                .take(self.dim - y)
                .map(|&c| c)
                .collect();
            total += count_words(&slash);
        }

        return total;
    }

    fn count_words(&self) -> usize {
        self.count_horrizontal()
            + self.count_vertical()
            + self.count_backslash()
            + self.count_forwardslash()
    }
}

fn lookup_xmas_skip(b: u8) -> usize {
    match b {
        0x58u8 => 3,
        0x4Du8 => 2,
        0x41u8 => 1,
        0x53u8 => 0,
        _ => panic!("unhandled byte? {:?}", b),
    }
}

fn lookup_samx_skip(b: u8) -> usize {
    match b {
        0x53u8 => 3,
        0x41u8 => 2,
        0x4Du8 => 1,
        0x58u8 => 0,
        _ => panic!("unhandled byte? {:?}", b),
    }
}

fn check_match(lhs: &[u8], rhs: &[u8]) -> bool {
    lhs == rhs
}

fn count_words(v: &Vec<u8>) -> usize {
    // Always going to look at the end
    let mut i = XMAS.len() - 1;
    let mut matches = 0;

    while i < v.len() {
        let xmas_skip = lookup_xmas_skip(v[i]);
        let samx_skip = lookup_samx_skip(v[i]);

        if xmas_skip == 0 {
            // Potentally matching XMAS
            if check_match(&v[i - (XMAS.len() - 1)..i + 1], &XMAS) {
                // Got one, next possible match is on the S so skip to that
                matches += 1;
                i += 3;
            } else {
                // No match, skip by the samx_skip (will always be the smaller
                // of the skips!
                i += samx_skip;
            }
        } else if samx_skip == 0 {
            // Potentally matching SAMX
            if check_match(&v[i - (XMAS.len() - 1)..i + 1], &SAMX) {
                // Got one, next possible match is on the X so skip to that
                matches += 1;
                i += 3;
            } else {
                // No match, skip by the xmas_skip (will always be the smaller
                // of the skips)!
                i += xmas_skip;
            }
        } else {
            // Matching neither, move on by min skip
            i += std::cmp::min(xmas_skip, samx_skip);
        }
    }

    return matches;
}

fn main() {
    let input = std::fs::read_to_string("inputs/day04.txt").expect("failed to read input");

    let g: Grid = input.parse().expect("failed to parse grid");

    println!("Part1 word count: {:?}", g.count_words());
}

#[cfg(test)]
mod day04_tests {
    use super::*;
    use rstest::rstest;

    const EXAMPLE_1_INPUT: &'static str = "\
        MMMSXXMASM\n\
        MSAMXMSMSA\n\
        AMXSXMAAMM\n\
        MSAMASMSMX\n\
        XMASAMXAMM\n\
        XXAMMXXAMA\n\
        SMSMSASXSS\n\
        SAXAMASAAA\n\
        MAMMMXMMMM\n\
        MXMXAXMASX\n\
        ";

    fn example_1() -> Grid {
        EXAMPLE_1_INPUT
            .parse()
            .expect("failed to parse example grid")
    }

    #[rstest]
    #[case("XMAS", 1)]
    #[case("SAMX", 1)]
    #[case("SAMXMAS", 2)]
    #[case("SAMXXMAS", 2)]
    #[case("XXXMAAS", 0)]
    fn test_matcher(#[case] input: &str, #[case] expected_count: usize) {
        assert_eq!(count_words(&input.bytes().collect()), expected_count);
    }

    #[rstest]
    fn test_parse_grid() {
        let g = example_1();
        assert_eq!(g.dim, 10);
    }

    #[rstest]
    fn test_example_1() {
        let g = example_1();

        assert_eq!(g.count_words(), 18);
    }

    #[rstest]
    fn test_example_1_horrizontal() {
        let g = example_1();

        assert_eq!(g.count_horrizontal(), 5);
    }

    #[rstest]
    fn test_example_1_vertical() {
        let g = example_1();

        assert_eq!(g.count_vertical(), 3);
    }

    #[rstest]
    fn test_example_1_backslash() {
        let g = example_1();

        assert_eq!(g.count_backslash(), 5);
    }

    #[rstest]
    fn test_example_1_forwardslash() {
        let g = example_1();

        assert_eq!(g.count_forwardslash(), 5);
    }
}
