use std::collections::HashMap;
use std::convert::TryInto;
use std::env;
use std::fs;

// part 2 of the puzzle
fn get_any_count(group: &str) -> i64 {
    // get all unique letters in a group
    let mut clear_group = group
        .chars()
        .filter(|&c| c.is_ascii_lowercase())
        .collect::<Vec<char>>();

    clear_group.sort_unstable();
    clear_group.dedup();
    clear_group.len() as i64
}

// part 2 of the puzzle
fn get_all_count(group: &str) -> i64 {
    let mut letter_counts: HashMap<char, i32> = HashMap::new();
    // for a letter to be accepted it has to occur in every line
    // I assume there are no duplicates on each line
    let lines: i64 = (group.split("\n").filter(|line| line.len() > 0).count())
        .try_into()
        .unwrap();
    for c in group.chars().collect::<Vec<char>>() {
        *letter_counts.entry(c).or_insert(0) += 1;
    }
    // count valid characters that appear lines times
    letter_counts.iter().fold(0, |acc, (key, val)| {
        if key.is_ascii_lowercase() && *val as i64 == lines {
            acc + 1
        } else {
            acc
        }
    }) as i64
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if args.len() < 2 {
        "data.txt"
    } else {
        args.get(1).unwrap()
    };
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let mut any_sum: i64 = 0;
    let mut all_sum: i64 = 0;
    for group in contents.split("\n\n").filter(|l| l.len() > 0) {
        any_sum += get_any_count(&group);
        all_sum += get_all_count(&group);
    }
    println!("Sum of any counts is {}", any_sum);
    println!("Sum of all counts is {}", all_sum);
}
