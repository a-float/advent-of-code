use std::env;
use std::fs;

const TREE: char = '#';

#[derive(Debug)]
struct Slope {
    x: usize,
    y: usize,
}

// walk the slope and count the encountered trees
fn count_trees_on_slope(map: &Vec<&str>, width: usize, slope: &Slope) -> i32 {
    let mut trees = 0;
    let mut x = 0;
    for (y, line) in map.iter().enumerate() {
        // skip line if y does not belong to the slope
        if y % slope.y != 0 || line.len() == 0 {
            continue;
        }
        if line.chars().nth(x % width).unwrap() == TREE {
            trees += 1;
        }
        x += slope.x;
    }
    trees
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = if args.len() < 2 {
        "data.txt"
    } else {
        args.get(1).unwrap()
    };
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let map: Vec<&str> = contents.split("\n").collect();
    let width = map.get(0).unwrap().len();
    let slopes = [
        Slope { x: 1, y: 1 },
        Slope { x: 3, y: 1 },
        Slope { x: 5, y: 1 },
        Slope { x: 7, y: 1 },
        Slope { x: 1, y: 2 },
    ];

    let mut tree_mult: i64 = 1;
    for slope in slopes.iter() {
        let trees = count_trees_on_slope(&map, width, slope);
        tree_mult *= trees as i64;
        println!("Found {: >3} trees on {:?}", trees, slope)
    }
    println!("Multiplied result is {}", tree_mult);
}
