use std::collections::{HashMap, HashSet};
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(PartialEq, Debug)]
struct Rule {
    before: u16,
    page: u16,
}

fn rule(before: u16, page: u16) -> Rule {
    Rule { before, page }
}

#[derive(Debug)]
struct RuleParseError;

impl From<ParseIntError> for RuleParseError {
    fn from(_: ParseIntError) -> Self {
        RuleParseError {}
    }
}

impl FromStr for Rule {
    type Err = RuleParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let result: Result<Vec<u16>, _> = s.split("|").take(2).map(|n| n.parse()).collect();
        let parts = result?;

        if parts.len() != 2 {
            return Err(Self::Err {});
        }

        return Ok(rule(parts[0], parts[1]));
    }
}

type CantFollow = HashMap<u16, Vec<u16>>;

// Create map of page num -> pages that cannot come after it
fn parse_order_rule_section(input: &str) -> CantFollow {
    let rule_iter = input
        .split('\n')
        .take_while(|line| !line.is_empty())
        .map(|rule_str| rule_str.parse::<Rule>().expect("failed to parse a rule"));

    let mut disallowed: CantFollow = HashMap::new();

    for rule in rule_iter {
        disallowed
            .entry(rule.page)
            .or_insert(Vec::new())
            .push(rule.before);
    }

    return disallowed;
}

fn parse_print_instructions(input: &str) -> Vec<Vec<u16>> {
    input
        .split('\n')
        .skip_while(|line| !line.is_empty())
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split(',')
                .map(|n| n.parse::<u16>().expect("failed to parse instruction"))
                .collect()
        })
        .collect()
}

fn correct_order(manual_print_order: &Vec<u16>, rules: &CantFollow) -> bool {
    manual_print_order
        .iter()
        .scan(HashSet::<u16>::new(), |cannot_appear, page| {
            if cannot_appear.contains(page) {
                // Rule violated
                return Some(false);
            }

            if let Some(l) = rules.get(page) {
                cannot_appear.extend(l.iter());
            }

            return Some(true);
        })
        .all(|x| x)
}

fn fix_order(manual_print_order: &Vec<u16>, rules: &CantFollow) -> Vec<u16> {
    let mut reversed_order = Vec::<u16>::new();

    for inst in manual_print_order.iter() {
        let insert_at = reversed_order
            .iter()
            .enumerate()
            .find(|(_, x)| match rules.get(x) {
                Some(cant_follow) => return cant_follow.contains(inst),
                None => false,
            })
            .map(|(i, _)| i)
            .or(Some(reversed_order.len()))
            .unwrap();

        reversed_order.insert(insert_at, *inst);
    }

    return reversed_order;
}

fn sum_correct_middles(manuals: &Vec<Vec<u16>>, rules: &CantFollow) -> u16 {
    manuals
        .iter()
        .filter(|manual_print_order| correct_order(manual_print_order, rules))
        .map(|manual_print_order| manual_print_order[manual_print_order.len() / 2])
        .sum()
}

fn fix_incorrect_and_sum(manuals: &Vec<Vec<u16>>, rules: &CantFollow) -> u16 {
    manuals
        .iter()
        .filter(|manual_print_order| !correct_order(manual_print_order, rules))
        .map(|manual_print_order| fix_order(manual_print_order, rules))
        .map(|manual_print_order| manual_print_order[manual_print_order.len() / 2])
        .sum()
}

fn main() {
    let input = std::fs::read_to_string("inputs/day05.txt").expect("failed to read input");

    let rules = parse_order_rule_section(&input);
    let manuals = parse_print_instructions(&input);

    println!(
        "Sum of middle pages for correctly ordered manuals: {:?}",
        sum_correct_middles(&manuals, &rules)
    );
    println!(
        "Sum of middle pages for fixed manuals: {:?}",
        fix_incorrect_and_sum(&manuals, &rules)
    );
}

#[cfg(test)]
mod day05_tests {
    use super::*;
    use rstest::rstest;

    const EXAMPLE_1_INPUT: &'static str = "\
    47|53\n\
    97|13\n\
    97|61\n\
    97|47\n\
    75|29\n\
    61|13\n\
    75|53\n\
    29|13\n\
    97|29\n\
    53|29\n\
    61|53\n\
    97|53\n\
    61|29\n\
    47|13\n\
    75|47\n\
    97|75\n\
    47|61\n\
    75|61\n\
    47|29\n\
    75|13\n\
    53|13\n\
         \n\
    75,47,61,53,29\n\
    97,61,53,29,13\n\
    75,29,13\n\
    75,97,47,61,53\n\
    61,13,29\n\
    97,13,75,29,47\n\
    ";

    #[rstest]
    #[case("47|55", rule(47, 55))]
    #[case("7|551", rule(7, 551))]
    fn test_parse_order_rule(#[case] input: &str, #[case] expected: Rule) {
        let actual: Rule = input.parse().expect("failed to parse");

        assert_eq!(expected, actual);
    }

    #[rstest]
    fn test_parse_order_rule_section() {
        let input = "\
        5|9\n\
        7|11\n\
        100|3\n\
        100|9\n\
        \n\
        garbage
        ";

        let rules = parse_order_rule_section(&input);

        assert_eq!(
            *rules.get(&9u16).expect("missing key: 9"),
            vec![5u16, 100u16]
        );
        assert_eq!(*rules.get(&11u16).expect("missing key: 7"), vec![7u16]);
        assert_eq!(*rules.get(&3u16).expect("missing key: 3"), vec![100u16]);
    }

    #[rstest]
    fn test_parse_print_instructions() {
        let input = "\
        garbage\n\
        garbage\n\
        \n\
        1,2,3,4\n\
        5,6,7,8\n\
        ";

        let orders = parse_print_instructions(&input);
        assert_eq!(
            vec![vec![1u16, 2u16, 3u16, 4u16], vec![5u16, 6u16, 7u16, 8u16]],
            orders
        );
    }

    #[rstest]
    fn test_example_part_1() {
        let rules = parse_order_rule_section(&EXAMPLE_1_INPUT);
        let manuals = parse_print_instructions(&EXAMPLE_1_INPUT);

        assert_eq!(143, sum_correct_middles(&manuals, &rules));
    }

    #[rstest]
    #[case(vec![75u16, 97u16, 47u16, 61u16, 53u16], vec![97u16, 75u16, 47u16, 61u16, 53u16])]
    #[case(vec![97u16, 75u16, 61u16, 47u16, 53u16], vec![97u16, 75u16, 47u16, 61u16, 53u16])]
    #[case(vec![61u16, 13u16, 29u16], vec![61u16, 29u16, 13u16])]
    #[case(vec![97u16, 13u16, 75u16, 29u16, 47u16], vec![97u16, 75u16, 47u16, 29u16, 13u16])]
    fn test_fix_incorrect(#[case] incorrect: Vec<u16>, #[case] expected: Vec<u16>) {
        let rules = parse_order_rule_section(&EXAMPLE_1_INPUT);
        let actual = fix_order(&incorrect, &rules);

        assert_eq!(expected, actual);
    }

    fn test_example_part_2() {
        let rules = parse_order_rule_section(&EXAMPLE_1_INPUT);
        let manuals = parse_print_instructions(&EXAMPLE_1_INPUT);

        assert_eq!(123, fix_incorrect_and_sum(&manuals, &rules));
    }
}
