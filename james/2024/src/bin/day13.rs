use adventofcode::util::Point;

#[derive(Debug, PartialEq, Eq)]
struct ClawMachine {
    prize: Point,
    a_button: Point,
    b_button: Point,
}

#[derive(Debug, PartialEq, Eq)]
struct ParseClawMachineError;

fn expect_token<'a>(s: &mut impl Iterator<Item = &'a str>, expected: &str) -> bool {
    let next_token = s.next();

    match next_token {
        Some(token) => token == expected,
        None => false,
    }
}

fn parse_prefixed<'a, I: std::str::FromStr>(
    s: &mut impl Iterator<Item = &'a str>,
    prefix: &str,
) -> Result<I, ParseClawMachineError> {
    // Cheeky, allow for trailing commas :)
    let token = s
        .next()
        .ok_or(ParseClawMachineError {})?
        .trim_end_matches(",");

    match token.strip_prefix(prefix) {
        Some(inner) => inner.parse().map_err(|_| ParseClawMachineError {}),
        None => Err(ParseClawMachineError {}),
    }
}

fn button_line(label: &str, s: &str) -> Result<Point, ParseClawMachineError> {
    let mut parts = s.split_whitespace();

    if !expect_token(&mut parts, "Button") {
        return Err(ParseClawMachineError {});
    }

    if !expect_token(&mut parts, &format!("{}:", label)) {
        return Err(ParseClawMachineError {});
    }

    Ok(Point {
        x: parse_prefixed(&mut parts, "X+")?,
        y: parse_prefixed(&mut parts, "Y+")?,
    })
}

fn prize_line(s: &str) -> Result<Point, ParseClawMachineError> {
    let mut parts = s.split_whitespace();

    if !expect_token(&mut parts, "Prize:") {
        return Err(ParseClawMachineError {});
    }

    Ok(Point {
        x: parse_prefixed(&mut parts, "X=")?,
        y: parse_prefixed(&mut parts, "Y=")?,
    })
}

impl std::str::FromStr for ClawMachine {
    type Err = ParseClawMachineError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();

        let a_button = button_line("A", lines.next().ok_or(Self::Err {})?)?;
        let b_button = button_line("B", lines.next().ok_or(Self::Err {})?)?;
        let prize = prize_line(lines.next().ok_or(Self::Err {})?)?;

        Ok(ClawMachine {
            a_button,
            b_button,
            prize,
        })
    }
}

impl std::fmt::Display for ClawMachine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "Button A: X+{}, Y+{}", self.a_button.x, self.a_button.y)?;
        writeln!(f, "Button B: X+{}, Y+{}", self.b_button.x, self.b_button.y)?;
        writeln!(f, "Prize: X={}, Y={}", self.prize.x, self.prize.y)
    }
}

impl ClawMachine {
    fn b_presses_required(&self) -> Option<usize> {
        // Derived from the two equasions:
        // x = Pa * Ax + Pb * Bx
        // y = Pa * Ay + Pb * By
        // ie x is defined by the the number of times A is pressed times by how far A moves in the
        // x plus the same for B...
        // If this results in a whole number then it is possible

        let (ax, ay) = <(f64, f64)>::try_from(self.a_button).ok()?;
        let (bx, by) = <(f64, f64)>::try_from(self.b_button).ok()?;
        let (x, y) = <(f64, f64)>::try_from(self.prize).ok()?;

        let b_presses = (ay * x - ax * y) / (bx * ay - ax * by);

        // If b_presses is a whole number, then we have a solution
        if b_presses.fract() == 0.0 {
            Some(b_presses as usize)
        } else {
            None
        }
    }

    fn min_presses_to_win(&self) -> Option<(usize, usize)> {
        let b_presses = self.b_presses_required()?;

        // a presses can be derived from the formuala
        // x = Pa * Ax + Pb * Bx
        let ax = self.a_button.x as f64;
        let bx = self.b_button.x as f64;
        let pb = b_presses as f64;
        let x = self.prize.x as f64;

        let a_presses = (x - pb * bx) / ax;

        if a_presses.fract() == 0.0 {
            Some((a_presses as usize, b_presses))
        } else {
            None
        }
    }

    fn min_cost_to_win(&self) -> Option<usize> {
        let (a_presses, b_presses) = self.min_presses_to_win()?;

        Some(a_presses * 3 + b_presses)
    }

    fn fix_prize_location(self) -> ClawMachine {
        const OFFSET: i64 = 10000000000000;
        ClawMachine {
            a_button: self.a_button,
            b_button: self.b_button,
            prize: Point {
                x: self.prize.x + OFFSET,
                y: self.prize.y + OFFSET,
            },
        }
    }
}

fn main() {
    let machines: Vec<ClawMachine> = std::fs::read_to_string("inputs/day13.txt")
        .expect("input file should be readable")
        .split("\n\n")
        .map(str::parse)
        .map(Result::unwrap)
        .collect();

    println!(
        "Minimum tokens to win: {}",
        machines
            .iter()
            .filter_map(ClawMachine::min_cost_to_win)
            .sum::<usize>()
    );
    println!(
        "Minimum tokens to win (fixed): {}",
        machines
            .into_iter()
            .map(ClawMachine::fix_prize_location)
            .filter_map(|c| c.min_cost_to_win())
            .sum::<usize>()
    );
}

#[cfg(test)]
mod day13_tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    fn test_parse_claw_machine() {
        let input = "\
            Button A: X+16, Y+17\n\
            Button B: X+19, Y+20\n\
            Prize: X=12, Y=11\n\
        ";

        let expected = ClawMachine {
            prize: Point { x: 12, y: 11 },
            a_button: Point { x: 16, y: 17 },
            b_button: Point { x: 19, y: 20 },
        };

        assert_eq!(
            expected,
            input
                .parse::<ClawMachine>()
                .expect("claw machine should parse")
        );
    }

    #[rstest]
    fn test_part_1_example_1() {
        let machine = ClawMachine {
            prize: Point { x: 8400, y: 5400 },
            a_button: Point { x: 94, y: 34 },
            b_button: Point { x: 22, y: 67 },
        };

        assert_eq!(Some(40), machine.b_presses_required());
        assert_eq!(Some((80, 40)), machine.min_presses_to_win());
        assert_eq!(Some(280), machine.min_cost_to_win());
    }

    #[rstest]
    fn test_part_1_example_2() {
        let machine = ClawMachine {
            prize: Point { x: 12748, y: 12176 },
            a_button: Point { x: 26, y: 66 },
            b_button: Point { x: 67, y: 21 },
        };

        assert_eq!(None, machine.b_presses_required());
        assert_eq!(None, machine.min_presses_to_win());
        assert_eq!(None, machine.min_cost_to_win());
    }

    #[rstest]
    fn test_part_1_example_3() {
        let machine = ClawMachine {
            prize: Point { x: 7870, y: 6450 },
            a_button: Point { x: 17, y: 86 },
            b_button: Point { x: 84, y: 37 },
        };

        assert_eq!(Some(86), machine.b_presses_required());
        assert_eq!(Some((38, 86)), machine.min_presses_to_win());
        assert_eq!(Some(200), machine.min_cost_to_win());
    }

    #[rstest]
    fn test_part_1_example_4() {
        let machine = ClawMachine {
            prize: Point { x: 18641, y: 10279 },
            a_button: Point { x: 69, y: 23 },
            b_button: Point { x: 27, y: 71 },
        };

        assert_eq!(None, machine.b_presses_required());
        assert_eq!(None, machine.min_presses_to_win());
        assert_eq!(None, machine.min_cost_to_win());
    }

    #[rstest]
    fn test_part_2_example_1() {
        let machine = ClawMachine {
            prize: Point { x: 8400, y: 5400 },
            a_button: Point { x: 94, y: 34 },
            b_button: Point { x: 22, y: 67 },
        }
        .fix_prize_location();

        assert_eq!(None, machine.b_presses_required());
        assert_eq!(None, machine.min_presses_to_win());
        assert_eq!(None, machine.min_cost_to_win());
    }

    #[rstest]
    fn test_part_2_example_2() {
        let machine = ClawMachine {
            prize: Point { x: 12748, y: 12176 },
            a_button: Point { x: 26, y: 66 },
            b_button: Point { x: 67, y: 21 },
        }
        .fix_prize_location();

        assert_eq!(Some(103199174542), machine.b_presses_required());
        assert_eq!(
            Some((118679050709, 103199174542)),
            machine.min_presses_to_win()
        );
        assert_eq!(Some(459236326669), machine.min_cost_to_win());
    }

    #[rstest]
    fn test_part_2_example_3() {
        let machine = ClawMachine {
            prize: Point { x: 7870, y: 6450 },
            a_button: Point { x: 17, y: 86 },
            b_button: Point { x: 84, y: 37 },
        }
        .fix_prize_location();

        assert_eq!(None, machine.b_presses_required());
        assert_eq!(None, machine.min_presses_to_win());
        assert_eq!(None, machine.min_cost_to_win());
    }

    #[rstest]
    fn test_part_2_example_4() {
        let machine = ClawMachine {
            prize: Point { x: 18641, y: 10279 },
            a_button: Point { x: 69, y: 23 },
            b_button: Point { x: 27, y: 71 },
        }
        .fix_prize_location();

        assert_eq!(Some(107526881786), machine.b_presses_required());
        assert_eq!(
            Some((102851800151, 107526881786)),
            machine.min_presses_to_win()
        );
        assert_eq!(Some(416082282239), machine.min_cost_to_win());
    }
}
