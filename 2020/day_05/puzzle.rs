use std::cmp;
use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if args.len() < 2 {
        "data.txt"
    } else {
        args.get(1).unwrap()
    };
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let mut max_id = 0;
    let mut taken_seats = HashSet::new();
    for line in contents.split("\n").filter(|l| l.len() > 0) {
        let mut row: i32 = 0;
        let mut col: i32 = 0;
        for c in line.chars() {
            match c {
                'F' => row = row << 1,
                'B' => row = row << 1 | 1,
                'R' => col = col << 1 | 1,
                'L' => col = col << 1,
                _ => println!("Unknown char {}", c),
            }
        }
        // println!("For seat {} got row {} and col {}", line, row, col);
        let seat_id = row * 8 + col;
        max_id = cmp::max(max_id, seat_id);
        taken_seats.insert(seat_id);
    }
    println!("Max id is {max_id}");

    // find my seat's id for part II
    for i in 0..=max_id {
        if !taken_seats.contains(&i)
            && taken_seats.contains(&(i - 1))
            && taken_seats.contains(&(i + 1))
        {
            println!("My seat has id of {i}");
            break;
        }
    }
}
