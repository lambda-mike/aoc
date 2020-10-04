use std::fs;

mod day03 {
    use std::collections::{HashMap, HashSet};

    pub type ClaimId = i32;

    #[derive(Debug, PartialEq, Eq, Hash)]
    pub struct Pos {
        pub x: i32,
        pub y: i32,
    }

    type ClaimsReport = HashMap<Pos, HashSet<ClaimId>>;

    pub fn solve_a(input: &str) -> usize {
        let claims_report = create_claims_report(input);
        count_overlapping_positions(&claims_report)
    }

    pub fn solve_b(input: &str) -> i32 {
        let claims_report = create_claims_report(input);
        find_not_overlapping_claim(&claims_report)
    }

    // #1 @ 265,241: 16x26
    pub fn parse_claim(line: &str) -> Vec<(Pos, ClaimId)> {
        let chunks = line.split(" ").collect::<Vec<&str>>();
        let id = chunks[0][1..].parse::<i32>().unwrap();
        let pos = chunks[2].split(":").collect::<Vec<&str>>()[0]
            .split(",")
            .collect::<Vec<&str>>();
        let x = pos[0].parse::<i32>().unwrap();
        let y = pos[1].parse::<i32>().unwrap();
        let dim = chunks[3].split("x").collect::<Vec<&str>>();
        let w = dim[0].parse::<i32>().unwrap();
        let h = dim[1].parse::<i32>().unwrap();
        let mut claims = vec![];
        for y in y..y + h {
            for x in x..x + w {
                claims.push((Pos { x: x, y: y }, id));
            }
        }
        claims
    }

    pub fn parse_claims(input: &str) -> Vec<(Pos, ClaimId)> {
        input
            .split("\n")
            .filter(|l| !l.is_empty())
            .flat_map(|line| parse_claim(line))
            .collect::<Vec<(Pos, ClaimId)>>()
    }

    pub fn analyze_claims(claims: Vec<(Pos, ClaimId)>) -> ClaimsReport {
        let mut dict: ClaimsReport = HashMap::new();
        for (pos, id) in claims {
            match dict.get_mut(&pos) {
                None => {
                    let mut set: HashSet<ClaimId> = HashSet::new();
                    set.insert(id);
                    dict.insert(pos, set);
                }
                Some(set) => {
                    set.insert(id);
                }
            }
        }
        dict
    }

    pub fn create_claims_report(input: &str) -> ClaimsReport {
        analyze_claims(parse_claims(input))
    }

    pub fn count_overlapping_positions(report: &ClaimsReport) -> usize {
        report.values().filter(|set| set.len() > 1).count()
    }

    pub fn find_not_overlapping_claim(report: &ClaimsReport) -> ClaimId {
        let mut overlapping: HashSet<ClaimId> = HashSet::new();
        let mut isolated: HashSet<ClaimId> = HashSet::new();
        for (_, ids) in report {
            if ids.len() > 1 {
                for id in ids.iter() {
                    overlapping.insert(*id);
                    isolated.remove(id);
                }
            } else {
                for id in ids.iter() {
                    if !overlapping.contains(id) {
                        isolated.insert(*id);
                    }
                }
            }
        }
        assert_eq!(
            isolated.len(),
            1,
            "There should be only one isolated claim!"
        );
        *isolated.iter().nth(0).unwrap()
    }
}

#[cfg(test)]
mod test_day03 {

    use super::day03::*;

    #[test]
    pub fn test_parse_claim() {
        let input = "#123 @ 3,2: 5x4";
        let result = parse_claim(input);
        let expected = "3 2 123;4 2 123;5 2 123;6 2 123;7 2 123;3 3 123;4 3 123;5 3 123;6 3 123;7 3 123;3 4 123;4 4 123;5 4 123;6 4 123;7 4 123;3 5 123;4 5 123;5 5 123;6 5 123;7 5 123";
        let result_str = result
            .iter()
            .map(|(pos, id)| format!("{} {} {}", pos.x, pos.y, id))
            .collect::<Vec<String>>()
            .join(";");
        assert_eq!(result.len(), 20);
        assert_eq!(result_str, expected);
    }

    #[test]
    pub fn test_parse_claims() {
        let input = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2";
        let result = parse_claims(input);
        let expected = "1 3 1;2 3 1;3 3 1;4 3 1;1 4 1;2 4 1;3 4 1;4 4 1;1 5 1;2 5 1;3 5 1;4 5 1;1 6 1;2 6 1;3 6 1;4 6 1;3 1 2;4 1 2;5 1 2;6 1 2;3 2 2;4 2 2;5 2 2;6 2 2;3 3 2;4 3 2;5 3 2;6 3 2;3 4 2;4 4 2;5 4 2;6 4 2;5 5 3;6 5 3;5 6 3;6 6 3";
        let result_str = result
            .iter()
            .map(|(pos, id)| format!("{} {} {}", pos.x, pos.y, id))
            .collect::<Vec<String>>()
            .join(";");
        assert_eq!(result.len(), 36);
        assert_eq!(result_str, expected);
    }

    #[test]
    pub fn test_analyze_claims() {
        let input = vec![
            (Pos { x: 3, y: 2 }, 1),
            (Pos { x: 3, y: 3 }, 1),
            (Pos { x: 3, y: 3 }, 2),
            (Pos { x: 2, y: 3 }, 2),
        ];
        let result = analyze_claims(input);
        assert_eq!(result.get(&Pos { x: 3, y: 2 }).unwrap().len(), 1);
        assert_eq!(result.get(&Pos { x: 3, y: 3 }).unwrap().len(), 2);
        assert_eq!(result.get(&Pos { x: 2, y: 3 }).unwrap().len(), 1);
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    // 101781
    println!("Solving Day03A...");
    let result = crate::day03::solve_a(&input);
    println!("{}", result);

    // 909
    println!("Solving Day03B...");
    let result = crate::day03::solve_b(&input);
    println!("{}", result);
}
