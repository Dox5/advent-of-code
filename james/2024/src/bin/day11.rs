use std::cell::RefCell;
use std::collections::HashMap;

fn num_digits(num: u64) -> u32 {
    if num == 0 {
        1
    } else {
        num.ilog10() + 1
    }
}

fn is_even(num: u64) -> bool {
    num % 2 == 0
}

fn bisect(num: u64) -> Option<(u64, u64)> {
    if !is_even(num_digits(num) as u64) {
        return None;
    }

    // order of magnatude needed to bisect the number, eg for 1234 this will be
    // 10
    let order = 10u64.pow((num_digits(num) / 2) as u32);

    Some((num / order, num % order))
}

fn recursive_count(cache: &mut HashMap<SplitKey, usize>, value: u64, split_count: usize) -> usize {
    // If we've seen the value at this depth before, then use the cached version
    let key = SplitKey {
        value,
        remaining_splits: split_count,
    };
    if let Some(&cached_count) = cache.get(&key) {
        return cached_count;
    }

    if split_count == 0 {
        return 1;
    }

    // Compute the number of stones from this point downward
    let stone_count = if value == 0 {
        recursive_count(cache, 1, split_count - 1)
    } else if let Some((lhs, rhs)) = bisect(value) {
        recursive_count(cache, lhs, split_count - 1) + recursive_count(cache, rhs, split_count - 1)
    } else {
        recursive_count(cache, value * 2024, split_count - 1)
    };

    // Cache the result for future use
    cache.insert(key, stone_count);

    return stone_count;
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
struct SplitKey {
    // The value that was split
    value: u64,
    // How many more splits happened after this one
    remaining_splits: usize,
}

struct StoneSplitter {
    cache: RefCell<HashMap<SplitKey, usize>>,
}

impl StoneSplitter {
    pub fn new() -> StoneSplitter {
        StoneSplitter {
            cache: RefCell::new(HashMap::with_capacity(128)),
        }
    }

    pub fn count_stones_after_splits(&self, value: &[u64], split_count: usize) -> usize {
        let mut cache = self.cache.borrow_mut();

        value
            .iter()
            .copied()
            .map(|value| recursive_count(&mut cache, value, split_count))
            .sum()
    }
}

fn main() {
    let input = std::fs::read_to_string("inputs/day11.txt").expect("failed to open input file");

    let sequence: Vec<u64> = input
        .split_whitespace()
        .map(|s| s.parse().expect("failed to parse value"))
        .collect();

    let splitter = StoneSplitter::new();

    println!(
        "Number of stones after 25 splits: {}",
        splitter.count_stones_after_splits(&sequence, 25)
    );
    println!(
        "Number of stones after 75 splits: {}",
        splitter.count_stones_after_splits(&sequence, 75)
    );
}

#[cfg(test)]
mod day11_tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(2024, 1, 2)]
    #[case(2024, 2, 4)]
    #[case(17, 0, 1)]
    #[case(17, 1, 2)]
    #[case(17, 2, 2)]
    #[case(17, 3, 3)]
    #[case(17, 4, 6)]
    #[case(17, 5, 8)]
    #[case(17, 6, 15)]
    fn test_stone_split_count(
        #[case] initial_value: u64,
        #[case] split_count: usize,
        #[case] expected: usize,
    ) {
        let splitter = StoneSplitter::new();

        assert_eq!(
            expected,
            splitter.count_stones_after_splits(&[initial_value], split_count)
        );
    }

    #[rstest]
    #[case(0, 1)]
    #[case(9, 1)]
    #[case(10, 2)]
    #[case(99, 2)]
    #[case(100, 3)]
    #[case(999, 3)]
    #[case(1000, 4)]
    #[case(9999, 4)]
    fn test_num_digits(#[case] num: u64, #[case] expected: u32) {
        assert_eq!(expected, num_digits(num));
    }

    #[rstest]
    #[case(5, None)]
    #[case(10, Some((1, 0)))]
    #[case(15, Some((1, 5)))]
    #[case(1000, Some((10, 0)))]
    #[case(1001, Some((10, 1)))]
    #[case(1234, Some((12, 34)))]
    #[case(12345, None)]
    #[case(28676032, Some((2867, 6032)))]
    fn test_bisect(#[case] num: u64, #[case] expected: Option<(u64, u64)>) {
        assert_eq!(expected, bisect(num));
    }

    #[rstest]
    fn test_example_1() {
        let sequence: &[u64] = &[125, 17];

        let splitter = StoneSplitter::new();
        assert_eq!(55312, splitter.count_stones_after_splits(&sequence, 25));
    }
}
