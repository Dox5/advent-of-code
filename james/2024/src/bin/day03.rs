#[derive(Eq, PartialEq, Copy, Clone, Debug)]
struct MulInst {
    a: u32,
    b: u32,
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum ParseMode {
    Basic,
    Accurate,
}

struct MulInstIter<'a> {
    input: std::iter::Peekable<std::str::Chars<'a>>,
    parse_mode: ParseMode,
    enable: bool,
}

impl MulInstIter<'_> {
    // Consumes exactly the literal given, otherwise fails
    // returns the number of characters consumed, or none of none were
    fn literal<'k>(&mut self, lit: &'k str) -> Result<&'k str, usize> {
        //println!("looking for {lit}");
        let mut consumed = 0;
        for expected in lit.chars() {
            //println!("checking {expected}");
            let &actual = self.input.peek().ok_or(consumed)?;

            if actual == expected {
                // matching
                self.input.next();
                consumed += 1;
            } else {
                // Next character doesn't match, fail
                //println!("{lit} doesn't match {actual} != {expected}");
                return Err(consumed);
            }
        }

        // Iterated the whole of lit so must be a match
        //println!("matched literal {lit}");
        return Ok(lit);
    }

    // Consume characters until one of 'any_of' matches
    fn skip_until(&mut self, any_of: &str) -> Result<char, usize> {
        let mut consumed = 0;
        while let Some(&c) = self.input.peek() {
            if any_of.contains(c) {
                return Ok(c);
            }
            consumed += 1;
            self.input.next();
        }

        // Didn't find one before the end of the input
        return Err(consumed);
    }

    // Consumes input until a whole multiply instruction is found or parsing
    // fails
    fn mul_inst(&mut self) -> Option<MulInst> {
        match self.parse_mode {
            // If doing basic, just skip to the next m
            ParseMode::Basic => {
                self.skip_until("m").ok()?;
            }

            // for accurate mode, skip depends on mode
            ParseMode::Accurate => {
                if self.enable {
                    // Only care about a mul or don't
                    let found = self.skip_until("md").ok()?;
                    match found {
                        'm' => (),
                        'd' => {
                            self.literal("don't()").ok()?;
                            // No longer enabled
                            self.enable = false;
                            // Cheeky, get the outter loop to retry at new
                            // position
                            return None;
                        }
                        _ => {
                            panic!("impossible!");
                        }
                    }
                } else {
                    // Only thing we care about is a do
                    self.skip_until("d").ok()?;
                    self.literal("do()").ok()?;
                    // Now enabled
                    self.enable = true;
                    // Return to let the caller handle the retry at the new
                    // location. (cheeky)
                    return None;
                }
            }
        }

        // Always expect to be landed on a mul( here
        self.literal("mul(").ok()?;
        let a = self.integer()?;
        //println!("found a = {:?}", a);
        self.literal(",").ok()?;
        let b = self.integer()?;
        //println!("found b = {:?}", a);
        self.literal(")").ok()?;

        return Some(MulInst { a, b });
    }

    // Consumes characters that are digits until a non-digit is found. Leaves
    // the non-integer that was found
    fn integer(&mut self) -> Option<u32> {
        let mut result = 0u32;
        let mut consumed = false;

        while let Some(d) = self.input.peek() {
            //println!("Checking if {d} is a digit");
            if !d.is_digit(10) {
                if consumed {
                    return Some(result);
                } else {
                    return None;
                }
            }

            result *= 10;
            result += d.to_digit(10)?;

            // Consume the character to move on
            consumed = true;
            self.input.next();
        }

        if consumed {
            return Some(result);
        } else {
            return None;
        }
    }
}

impl Iterator for MulInstIter<'_> {
    type Item = MulInst;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(_) = self.input.peek() {
            let maybe_inst = self.mul_inst();
            if maybe_inst.is_some() {
                return maybe_inst;
            }
        }

        // Ran out of input
        return None;
    }
}

fn iter_instructions(input: &str, parse_mode: ParseMode) -> MulInstIter {
    return MulInstIter {
        input: input.chars().peekable(),
        parse_mode,
        enable: true,
    };
}

fn sum_of_product(input: &str, parse_mode: ParseMode) -> u32 {
    return iter_instructions(input, parse_mode)
        .map(|mul| mul.a * mul.b)
        .sum();
}

fn main() {
    let input = std::fs::read_to_string("inputs/day03.txt").expect("failed to read input");

    let part1 = sum_of_product(&input, ParseMode::Basic);
    println!("Part1: Sum of products with basic parsing: {part1}");

    let part2 = sum_of_product(&input, ParseMode::Accurate);
    println!("Part2: Sum of products with accurate parsing: {part2}");
}

#[cfg(test)]
mod day03_tests {
    use super::*;

    use rstest::rstest;

    const EXAMPLE_INPUT_1: &'static str =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    const EXAMPLE_INPUT_2: &'static str =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";

    #[rstest]
    #[case("mul(8,8)", vec![MulInst{a: 8, b: 8}])]
    #[case("mmul(8,8)", vec![MulInst{a: 8, b: 8}])]
    #[case("muul(8,8)", vec![])]
    #[case("mumul(8,8)", vec![MulInst{a: 8, b: 8}])]
    #[case("mu", vec![])]
    #[case("xxxx", vec![])]
    #[case("mul(200,5)mul(6,9)", vec![MulInst{a: 200, b: 5}, MulInst{a: 6, b: 9}])]
    #[case("mul(200,5) mul(6,9)", vec![MulInst{a: 200, b: 5}, MulInst{a: 6, b: 9}])]
    fn test_parse_multiply_instructions(#[case] input: &str, #[case] expected: Vec<MulInst>) {
        let actual: Vec<MulInst> = iter_instructions(input, ParseMode::Basic).collect();
        assert_eq!(expected, actual);
    }

    #[rstest]
    fn test_part1_example() {
        assert_eq!(sum_of_product(EXAMPLE_INPUT_1, ParseMode::Basic), 161);
    }

    #[rstest]
    fn test_part2_example() {
        assert_eq!(sum_of_product(EXAMPLE_INPUT_2, ParseMode::Accurate), 48);
    }
}
