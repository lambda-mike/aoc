use std::fs;

mod day01 {
    use std::collections::HashMap;

    pub fn parse_changes(input: &str) -> Vec<i32> {
        input
            .split('\n')
            .filter(|s| !s.is_empty())
            .map(|s| s.parse::<i32>().unwrap())
            .collect()
    }

    pub fn sum_freq_changes(freq_changes: &[i32]) -> i32 {
        freq_changes.iter().sum()
    }

    pub fn solve_a(input: &str) -> i32 {
        let freq_changes = parse_changes(input);
        sum_freq_changes(&freq_changes)
    }

    pub fn find_first_duplicate_frequency(freq_changes: &[i32]) -> i32 {
        let mut dict = HashMap::new();
        let mut current_freq = 0;
        for freqency_change in freq_changes.iter().cycle() {
            if dict.contains_key(&current_freq) {
                return current_freq;
            } else {
                dict.insert(current_freq, 0);
                current_freq += freqency_change;
            }
        }
        unreachable!("This code should never be hit!")
    }

    pub fn solve_b(input: &str) -> i32 {
        let freq_changes = parse_changes(input);
        find_first_duplicate_frequency(&freq_changes)
    }
}

#[cfg(test)]
mod test_day01 {

    mod parse_changes {
        use super::super::day01::*;

        #[test]
        fn parse_changes_1() {
            let input = "+1\n+1\n+1";
            assert_eq!(parse_changes(input), [1, 1, 1]);
        }
        #[test]
        fn parse_changes_2() {
            let input = "+1\n+1\n-2";
            assert_eq!(parse_changes(input), [1, 1, -2]);
        }
        #[test]
        fn parse_changes_3() {
            let input = "-1\n-2\n-3";
            assert_eq!(parse_changes(input), [-1, -2, -3]);
        }
    }

    mod sum_freq_changes {
        use super::super::day01::*;
        #[test]
        fn sum_freq_changes_0() {
            assert_eq!(sum_freq_changes(&[]), 0);
        }
        #[test]
        fn sum_freq_changes_1() {
            let fcs = [1, 1, 1];
            assert_eq!(sum_freq_changes(&fcs), 3);
        }
        #[test]
        fn sum_freq_changes_2() {
            let fcs = [1, 1, -2];
            assert_eq!(sum_freq_changes(&fcs), 0);
        }
        #[test]
        fn sum_freq_changes_3() {
            let fcs = [-1, -2, -3];
            assert_eq!(sum_freq_changes(&fcs), -6);
        }
    }

    mod solve_a {
        use super::super::day01::*;

        #[test]
        fn sample() {
            let input = "+1\n-2\n+3\n+1";
            let result = solve_a(input);
            assert_eq!(result, 3);
        }
    }

    mod find_first_duplicate_frequency {
        use super::super::day01::*;

        #[test]
        fn find_first_duplicate_frequency1() {
            let input = [1, -1];
            let result = find_first_duplicate_frequency(&input);
            assert_eq!(result, 0);
        }
        #[test]
        fn find_first_duplicate_frequency2() {
            let input = [3, 3, 4, -2, -4];
            let result = find_first_duplicate_frequency(&input);
            assert_eq!(result, 10);
        }
        #[test]
        fn find_first_duplicate_frequency3() {
            let input = [-6, 3, 8, 5, -6];
            let result = find_first_duplicate_frequency(&input);
            assert_eq!(result, 5);
        }
        #[test]
        fn find_first_duplicate_frequency4() {
            let input = [7, 7, -2, -7, -4];
            let result = find_first_duplicate_frequency(&input);
            assert_eq!(result, 14);
        }
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    // 510
    println!("Solving Day01A...");
    let result = crate::day01::solve_a(&input);
    println!("{}", result);

    // 69074
    println!("Solving Day01B...");
    let result = crate::day01::solve_b(&input);
    println!("{}", result);
}
