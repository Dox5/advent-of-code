use std::collections::HashMap;
use std::fs;
use std::hash::Hash;

fn parse_line(line: &str) -> (u32, u32) {
    let nums: Vec<u32> = line
        .split_whitespace()
        .map(|e| e.parse().unwrap())
        .collect();
    match nums[..] {
        [a, b] => (a, b),
        _ => panic!("Wrong number of columns on line!"),
    }
}

fn location_lists(input: &str) -> (Vec<u32>, Vec<u32>) {
    input
        .split('\n')
        .filter(|line| !line.is_empty())
        .map(parse_line)
        .unzip()
}

fn total_distance(input: &str) -> u32 {
    let (mut a, mut b) = location_lists(input);
    a.sort();
    b.sort();
    a.iter().zip(b.iter()).map(|(l, r)| l.abs_diff(*r)).sum()
}

fn count_unqiue<I>(iter: I) -> HashMap<I::Item, u32>
where
    I: Iterator,
    I::Item: Eq + Hash,
{
    let mut counts = HashMap::new();

    iter.for_each(|item| *counts.entry(item).or_default() += 1);

    return counts;
}

fn similarity_score(input: &str) -> u32 {
    let (a, b) = location_lists(input);

    let a_counts = count_unqiue(a.iter());
    let b_counts = count_unqiue(b.iter());

    let mut score = 0u32;

    for (&location, &a_count) in a_counts.iter() {
        let b_count = match b_counts.get(location) {
            Some(&c) => c,
            None => 0,
        };

        score += a_count * (location * b_count);
    }

    return score;
}

fn main() {
    let input = fs::read_to_string("inputs/day01.txt").expect("failed to read input");
    let distance = total_distance(&input);
    println!("Total distance {distance}");

    let score = similarity_score(&input);
    println!("Similarity score {score}");
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    const EXAMPLE_INPUT: &'static str = "3   4\n\
                 4   3\n\
                 2   5\n\
                 1   3\n\
                 3   9\n\
                 3   3\n\
                 ";

    #[rstest]
    fn test_parse_line() {
        let input = "4   5";
        assert_eq!(parse_line(input), (4, 5))
    }

    #[rstest]
    fn part1_example() {
        assert_eq!(total_distance(EXAMPLE_INPUT), 11);
    }

    #[rstest]
    fn part2_example() {
        assert_eq!(similarity_score(EXAMPLE_INPUT), 31);
    }
}
