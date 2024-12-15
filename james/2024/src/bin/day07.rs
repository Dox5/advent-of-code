use std::io;
use std::io::BufRead;
use std::str::FromStr;

#[derive(Eq, PartialEq, Debug)]
struct Calibration {
    test_value: i64,
    readings: Vec<i64>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct CalibrationParseError;

impl FromStr for Calibration {
    type Err = CalibrationParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split(&[' ', ':'])
            .filter(|l| !l.is_empty())
            .map(|e| e.parse())
            .collect::<Result<Vec<i64>, _>>()
            .map_err(|_| CalibrationParseError {})
            .map(|nums| Calibration {
                test_value: *nums.first().unwrap(),
                readings: Vec::from_iter(nums.into_iter().skip(1)),
            })
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Op {
    Add,
    Mul,
    Cat,
}

fn base10_order_mag(v: i64) -> i64 {
    let m = 10i64.pow(((v + 1) as f32).log(10f32).ceil() as u32);
    m
}

fn y(target: i64, readings: &[i64], allow_cat: bool) -> Option<Vec<Op>> {
    // Failure conditions
    if target <= 0 {
        // We would have already succeded
        return None;
    } else if readings.len() == 1 {
        if readings[0] != target {
            // Fail...
            return None;
        } else {
            // Success, we have hit the target
            return Some(Vec::new());
        }
    } else {
        let operand: i64 = *readings.last().unwrap();
        let next_readings = &readings[0..readings.len() - 1];

        let m = base10_order_mag(operand);

        // Try cat is allow (fastest to fail)
        if allow_cat && (target % m) == operand {
            // concat operation possilbe here, so try it
            if let Some(mut ops) = y(target / m, &next_readings, allow_cat) {
                ops.push(Op::Cat);
                return Some(ops);
            }
        }

        // Try division route second (2nd fastest to fail)
        if target % operand == 0 {
            if let Some(mut ops) = y(target / operand, &next_readings, allow_cat) {
                ops.push(Op::Mul);
                return Some(ops);
            }
        }

        // Can it subtract
        //
        // If we get here divide didn't work so try substract
        if let Some(mut ops) = y(target - operand, &next_readings, allow_cat) {
            ops.push(Op::Add);
            return Some(ops);
        }

        // If we got here, nothing worked
        return None;
    }
}

impl Calibration {
    fn x(&self) -> Option<Vec<Op>> {
        return y(self.test_value, &self.readings, false);
    }

    fn x_cat(&self) -> Option<Vec<Op>> {
        return y(self.test_value, &self.readings, true);
    }
}

fn read_calibrations<R: io::Read>(file: R) -> Vec<Calibration> {
    io::BufReader::new(file)
        .lines()
        .map(|l| l.unwrap().parse().expect("failed to parse Calibration"))
        .collect()
}

fn sum_possible_calibrations<'a, I>(calibrations: I) -> i64
where
    I: Iterator<Item = &'a Calibration>,
{
    calibrations
        .filter(|c| c.x().is_some())
        .map(|c| c.test_value)
        .sum()
}

fn sum_possible_calibrations_cat<'a, I>(calibrations: I) -> i64
where
    I: Iterator<Item = &'a Calibration>,
{
    calibrations
        .filter(|c| c.x_cat().is_some())
        .map(|c| c.test_value)
        .sum()
}

fn main() -> io::Result<()> {
    let file = std::fs::File::open("inputs/day07.txt")?;
    let calibrations = read_calibrations(file);

    let total_calibration_result = sum_possible_calibrations(calibrations.iter());
    println!(
        "Total calibration result for possible test values: {}",
        total_calibration_result
    );

    let total_calibration_result_cat = sum_possible_calibrations_cat(calibrations.iter());
    println!(
        "Total calibration result for possible test values (with cat): {}",
        total_calibration_result_cat
    );

    Ok(())
}

#[cfg(test)]
mod day07_tests {
    use super::*;
    use rstest::rstest;

    const EXAMPLE_INPUT: &'static str = "\
        190: 10 19\n\
        3267: 81 40 27\n\
        83: 17 5\n\
        156: 15 6\n\
        7290: 6 8 6 15\n\
        161011: 16 10 13\n\
        192: 17 8 14\n\
        21037: 9 7 18 13\n\
        292: 11 6 16 20\n\
    ";

    #[rstest]
    fn test_example_part_1() {
        assert_eq!(
            3749,
            sum_possible_calibrations(read_calibrations(EXAMPLE_INPUT.as_bytes()).iter())
        );
    }

    #[rstest]
    fn test_example_part_2() {
        assert_eq!(
            11387,
            sum_possible_calibrations_cat(read_calibrations(EXAMPLE_INPUT.as_bytes()).iter())
        );
    }

    #[rstest]
    #[case("190: 19 10", Calibration{test_value: 190, readings: vec![19, 10]})]
    #[case("7290: 6 8 6 15", Calibration{test_value: 7290, readings: vec![6, 8, 6, 15]})]
    fn test_parse_calibration(#[case] input: &str, #[case] expected: Calibration) {
        let actual: Calibration = input.parse().unwrap();
        assert_eq!(expected, actual);
    }

    #[rstest]
    #[case(Calibration{test_value: 190, readings: vec![19, 10]}, Some(vec![Op::Mul]))]
    #[case(Calibration{test_value: 3267, readings: vec![81, 40, 27]}, Some(vec![Op::Add, Op::Mul]))]
    #[case(Calibration{test_value: 7290, readings: vec![6, 8, 6, 15]}, None)]
    #[case(Calibration{test_value: 161011, readings: vec![16, 10, 13]}, None)]
    fn test_check_possible(#[case] calibration: Calibration, #[case] expected: Option<Vec<Op>>) {
        assert_eq!(expected, calibration.x());
    }

    #[rstest]
    #[case(Calibration{test_value: 283368, readings: vec![3, 8, 8, 322, 8]}, Some(vec![Op::Add, Op::Mul, Op::Mul, Op::Cat]))]
    #[case(Calibration{test_value: 4368, readings: vec![9, 87, 1, 8, 42]}, Some(vec![Op::Add, Op::Mul, Op::Add, Op::Mul]))]
    fn test_check_possible_cat(
        #[case] calibration: Calibration,
        #[case] expected: Option<Vec<Op>>,
    ) {
        assert_eq!(expected, calibration.x_cat());
    }

    use rand::{Rng, SeedableRng};

    #[rstest]
    fn test_property_test_cat() {
        let mut rng = rand::rngs::StdRng::seed_from_u64(181);

        let iterations = 1000;

        for _ in 0..iterations {
            let n_values: u8 = rng.gen_range(1..6);
            let n_operations: u8 = n_values - 1;
            let test_values: Vec<i64> = (0..)
                .take(n_values as usize)
                .map(|_| rng.gen_range(1..200))
                .collect();

            let test_ops: Vec<Op> = (0..)
                .take(n_operations as usize)
                .map(|_| rng.gen_range(0..3))
                .map(|i| match i {
                    0 => Op::Add,
                    1 => Op::Mul,
                    2 => Op::Cat,
                    _ => panic!("not expecting {:?}", i),
                })
                .collect();

            let calibration_res: i64 =
                test_values
                    .iter()
                    .skip(1)
                    .zip(&test_ops)
                    .fold(test_values[0], |acc, (v, op)| match op {
                        Op::Add => acc + v,
                        Op::Mul => acc * v,
                        Op::Cat => acc * base10_order_mag(*v) + v,
                    });

            let calibration = Calibration {
                test_value: calibration_res,
                readings: test_values,
            };

            println!("{:?}", calibration);
            assert_eq!(calibration.x_cat(), Some(test_ops));
        }
    }

    #[rstest]
    #[case(10, 100)]
    #[case(15, 100)]
    #[case(1, 10)]
    #[case(9, 10)]
    #[case(578, 1000)]
    #[case(999, 1000)]
    fn test_base10_mag(#[case] v: i64, #[case] expected: i64) {
        assert_eq!(expected, base10_order_mag(v));
    }
}
