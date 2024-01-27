use std::env;
use std::fs;

fn is_valid_1(min: i32, max: i32, letter: char, password: &str) -> bool {
    let mut count = 0;
    for c in password.chars() {
        if c == letter {
            count += 1;
        }
    }
    min <= count && count <= max
}

fn is_valid_2(min: i32, max: i32, letter: char, password: &str) -> bool {
    let first = min as usize - 1;
    let second = max as usize - 1;
    let first_ok = password.chars().nth(first).unwrap() == letter;
    let second_ok = password.chars().nth(second).unwrap() == letter;
    (first_ok && !second_ok) || (!first_ok && second_ok)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if args.len() < 2 {
        "data.txt"
    } else {
        args.get(1).unwrap()
    };
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let mut valid_count_1 = 0;
    let mut valid_count_2 = 0;

    for line in contents.split("\n") {
        if line.len() == 0 {
            continue;
        }
        let parts: Vec<&str> = line.split(" ").collect();
        let range: Vec<&str> = parts[0].split("-").collect();
        let min: i32 = range[0].parse().unwrap();
        let max: i32 = range[1].parse().unwrap();
        let letter = parts[1].chars().next().unwrap();
        let password = parts[2];
        // println!("{:?} {:?} {:?} {:?}", min, max, letter, password);
        if is_valid_1(min, max, letter, password) {
            valid_count_1 += 1;
        }
        if is_valid_2(min, max, letter, password) {
            valid_count_2 += 1;
        }
    }
    println!("Part 1. Number of valid passwords: {}", valid_count_1);
    println!("Part 2. Number of valid passwords: {}", valid_count_2);
}
