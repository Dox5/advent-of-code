use adventofcode::vector::Vector2D;
use std::collections::HashSet;
use std::str::FromStr;

const MAS: [u8; 3] = [0x4Du8, 0x41u8, 0x53u8];
const SAM: [u8; 3] = [0x53u8, 0x41u8, 0x4Du8];

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
    fn locate_backslash(&self) -> Vec<Vector2D> {
        let mut found: Vec<Vector2D> = Vec::new();

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

            locate_words(&slash)
                .into_iter()
                .map(|offset| Vector2D {
                    x: (x + offset).try_into().unwrap(),
                    y: offset.try_into().unwrap(),
                })
                .for_each(|p| found.push(p));
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

            locate_words(&slash)
                .into_iter()
                .map(|offset| Vector2D {
                    x: offset.try_into().unwrap(),
                    y: (y + offset).try_into().unwrap(),
                })
                .for_each(|p| found.push(p));
        }

        return found;
    }

    fn locate_forwardslash(&self) -> Vec<Vector2D> {
        let mut found: Vec<Vector2D> = Vec::new();

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

            locate_words(&slash)
                .into_iter()
                .map(|offset| Vector2D {
                    x: (x - offset).try_into().unwrap(),
                    y: offset.try_into().unwrap(),
                })
                .for_each(|p| found.push(p));
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

            locate_words(&slash)
                .into_iter()
                .map(|offset| Vector2D {
                    x: (self.dim - (offset + 1)).try_into().unwrap(),
                    y: (y + offset).try_into().unwrap(),
                })
                .for_each(|p| found.push(p));
        }

        return found;
    }

    fn count_crosses(&self) -> usize {
        let bslash: HashSet<Vector2D> = HashSet::from_iter(self.locate_backslash().into_iter());
        let fslash: HashSet<Vector2D> = HashSet::from_iter(self.locate_forwardslash().into_iter());

        return bslash.intersection(&fslash).count();
    }
}

fn lookup_mas_skip(b: u8) -> usize {
    match b {
        0x58u8 => 3,
        0x4Du8 => 2,
        0x41u8 => 1,
        0x53u8 => 0,
        _ => panic!("unhandled byte? {:?}", b),
    }
}

fn lookup_sam_skip(b: u8) -> usize {
    match b {
        0x58u8 => 3,
        0x53u8 => 2,
        0x41u8 => 1,
        0x4Du8 => 0,
        _ => panic!("unhandled byte? {:?}", b),
    }
}

fn check_match(lhs: &[u8], rhs: &[u8]) -> bool {
    lhs == rhs
}

// Return the set of offsets for where the A is on each match
fn locate_words(v: &Vec<u8>) -> Vec<usize> {
    // Always going to look at the end
    let mut i = MAS.len() - 1;
    let mut matches: Vec<usize> = Vec::new();

    while i < v.len() {
        let mas_skip = lookup_mas_skip(v[i]);
        let sam_skip = lookup_sam_skip(v[i]);

        if mas_skip == 0 {
            // Potentally matching MAS
            if check_match(&v[i - (MAS.len() - 1)..i + 1], &MAS) {
                // Got one, next possible match is on the S so skip to that
                matches.push(i - 1);
                i += 2;
            } else {
                // No match, skip by the samx_skip (will always be the smaller
                // of the skips!
                i += sam_skip;
            }
        } else if sam_skip == 0 {
            // Potentally matching SAMX
            if check_match(&v[i - (MAS.len() - 1)..i + 1], &SAM) {
                // Got one, next possible match is on the M so skip to that
                matches.push(i - 1);
                i += 2;
            } else {
                // No match, skip by the mas_skip (will always be the smaller
                // of the skips)!
                i += mas_skip;
            }
        } else {
            // Matching neither, move on by min skip
            i += std::cmp::min(mas_skip, sam_skip);
        }
    }

    return matches;
}

fn main() {
    let input = std::fs::read_to_string("inputs/day04.txt").expect("failed to read input");

    let g: Grid = input.parse().expect("failed to parse grid");

    println!("Part2 cross count: {:?}", g.count_crosses());
}

#[cfg(test)]
mod day04p2_tests {
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
    #[case("MAS", vec![1usize])]
    #[case("SAM", vec![1usize])]
    #[case("SAMAS", vec![1usize, 3usize])]
    #[case("SAMMAS", vec![1usize, 4usize])]
    #[case("XXXMAAS", vec![])]
    fn test_matcher(#[case] input: &str, #[case] expected_count: Vec<usize>) {
        assert_eq!(locate_words(&input.bytes().collect()), expected_count);
    }

    #[rstest]
    fn test_parse_grid() {
        let g = example_1();
        assert_eq!(g.dim, 10);
    }

    #[rstest]
    fn test_example_1() {
        let g = example_1();

        assert_eq!(g.count_crosses(), 9);
    }
}
