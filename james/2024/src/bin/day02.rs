use std::fs::read_to_string;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
struct Report {
    levels: Vec<u32>,
}

#[derive(Debug, PartialEq, Eq)]
struct ReportParseError;

impl From<ParseIntError> for ReportParseError {
    fn from(_error: ParseIntError) -> Self {
        ReportParseError {}
    }
}

impl FromStr for Report {
    type Err = ReportParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let result: Result<Vec<u32>, _> = s.split_whitespace().map(|n| n.parse::<u32>()).collect();

        Ok(Report { levels: result? })
    }
}

fn level_transition_safe(direction: i64, diff: i64) -> bool {
    if diff == 0 {
        // Not safe, repeated value
        return false;
    } else if diff.signum() != direction {
        // Not uniformly increasing/decreasing
        return false;
    } else if diff.abs() > 3 {
        // Too far apart
        return false;
    } else {
        return true;
    }
}

#[derive(PartialEq, Eq, Debug)]
enum ProblemDamper {
    Active,
    Inactive,
}

fn window(collection: &Vec<u32>) -> impl Iterator<Item = (u32, u32)> + '_ {
    return collection
        .iter()
        .zip(collection.iter().skip(1))
        .map(|(&x, &y)| (x, y));
}

fn all_safe<I>(iter: I) -> bool
where
    I: Iterator<Item = (u32, u32)>,
{
    let mut direction: i64 = 0;

    for (from, to) in iter {
        let diff: i64 = (from as i64) - (to as i64);

        if direction == 0 {
            // Not detected a direction yet
            direction = diff.signum();
        }

        if !level_transition_safe(direction, diff) {
            return false;
        }
    }

    return true;
}

impl Report {
    fn check_safe(&self, damper: ProblemDamper) -> bool {
        // First try undamped
        if all_safe(window(&self.levels)) {
            // Done!
            return true;
        }

        // No further checking to do without the damper
        if damper == ProblemDamper::Inactive {
            return false;
        }

        let max_len = self.levels.len();
        for skip_i in 0..max_len {
            let next_test = self
                .levels
                .iter()
                .take(skip_i)
                .chain(self.levels.iter().skip(skip_i + 1))
                .map(|x| *x)
                .collect::<Vec<u32>>();

            if all_safe(window(&next_test)) {
                return true;
            }
        }

        return false;
    }

    fn is_safe(&self) -> bool {
        self.check_safe(ProblemDamper::Inactive)
    }

    fn is_safe_damped(&self) -> bool {
        self.check_safe(ProblemDamper::Active)
    }
}

fn parse_reports(input: &str) -> Vec<Report> {
    let result: Result<Vec<_>, _> = input
        .split("\n")
        .filter(|l| !l.is_empty())
        .map(|l| l.parse())
        .collect();
    return result.expect("failed to parse a report");
}

fn count_safe(input: &str) -> usize {
    parse_reports(input)
        .iter()
        .filter(|report| report.is_safe())
        .count()
}

fn count_safe_damped(input: &str) -> usize {
    parse_reports(input)
        .iter()
        .filter(|report| report.is_safe_damped())
        .count()
}

fn main() {
    let input = read_to_string("inputs/day02.txt").expect("failed to read input");

    let n_safe = count_safe(&input);
    println!("Number of safe reports: {n_safe}");

    let n_safe_damped = count_safe_damped(&input);
    println!("Number of safe reports (with damping): {n_safe_damped}");
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    const EXAMPLE_INPUT: &'static str = "\
    7 6 4 2 1\n\
    1 2 7 8 9\n\
    9 7 6 2 1\n\
    1 3 2 4 5\n\
    8 6 4 4 1\n\
    1 3 6 7 9\n\
    ";

    #[rstest]
    fn test_parse_report() {
        let report_str = "7 6 4 2 1";
        let parsed_report: Report = report_str.parse().expect("failed to parse report");

        assert_eq!(
            parsed_report,
            Report {
                levels: vec![7, 6, 4, 2, 1]
            }
        );
    }

    #[rstest]
    fn test_parse_reports() {
        let report_str = "1 2 3\n7 6 5\n";
        assert_eq!(
            parse_reports(report_str),
            vec![
                Report {
                    levels: vec![1, 2, 3]
                },
                Report {
                    levels: vec![7, 6, 5]
                }
            ]
        );
    }

    #[rstest]
    #[case(Report{levels: vec![1, 2, 3]}, true)]
    #[case(Report{levels: vec![3, 2, 1]}, true)]
    #[case(Report{levels: vec![1, 4, 5]}, true)]
    #[case(Report{levels: vec![3, 3, 2]}, false)]
    #[case(Report{levels: vec![1, 5]}, false)]
    #[case(Report{levels: vec![1, 2, 1, 2]}, false)]
    #[case(Report{levels: vec![10, 9, 8, 4]}, false)]
    #[case(Report{levels: vec![1, 1, 1, 1]}, false)]
    fn test_is_safe(#[case] report: Report, #[case] is_safe: bool) {
        assert_eq!(report.is_safe(), is_safe);
    }

    #[rstest]
    fn test_example_part_1() {
        assert_eq!(count_safe(EXAMPLE_INPUT), 2);
    }

    #[rstest]
    #[case(Report{levels: vec![7, 6, 4, 2, 1]}, true)]
    #[case(Report{levels: vec![1, 2, 7, 8, 9]}, false)]
    #[case(Report{levels: vec![9, 7, 6, 2, 1]}, false)]
    #[case(Report{levels: vec![1, 3, 2, 4, 5]}, true)]
    #[case(Report{levels: vec![8, 6, 4, 4, 1]}, true)]
    #[case(Report{levels: vec![1, 3, 6, 7, 9]}, true)]
    #[case(Report{levels: vec![7, 8, 6, 5]}, true)]
    fn test_is_safe_damped(#[case] report: Report, #[case] is_safe: bool) {
        assert_eq!(report.is_safe_damped(), is_safe);
    }

    #[rstest]
    fn test_example_part_2() {
        assert_eq!(count_safe_damped(EXAMPLE_INPUT), 4);
    }
}
