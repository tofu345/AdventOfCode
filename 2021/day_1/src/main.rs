use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    part_two(&file_contents);
}

fn part_one(file_contents: &str) {
    let mut prev = None;
    let mut num_inc = 0;
    for line in file_contents.lines() {
        let curr = line.parse::<i32>().unwrap();
        if let Some(v) = prev {
            if curr > v {
                num_inc += 1;
            }
        }

        prev = Some(curr);
    }

    println!("Part One: {num_inc}");
}

fn part_two(file_contents: &str) {
    let mut prev = None;
    let mut num_inc = 0;
    let lines: Vec<_> = file_contents
        .lines()
        .map(|v| v.parse::<i32>().unwrap())
        .collect();

    for i in 2..lines.len() {
        let (x, y, z) = (lines[i], lines[i - 1], lines[i - 2]);
        let sum = x + y + z;
        if let Some(v) = prev {
            if sum > v {
                num_inc += 1;
            }
        }
        prev = Some(sum);
    }

    println!("Part Two: {num_inc}");
}
