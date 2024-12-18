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
    let mut depth = 0;
    let mut hor_pos = 0;
    for line in file_contents.lines() {
        let split: Vec<_> = line.split_whitespace().collect();
        let x: i32 = split[1].parse().unwrap();
        match split[0] {
            "forward" => hor_pos += x,
            "down" => depth += x,
            "up" => depth -= x,
            _ => unreachable!(),
        }
    }

    println!("Part One: {}", depth * hor_pos);
}

fn part_two(file_contents: &str) {
    let mut depth = 0;
    let mut hor_pos = 0;
    let mut aim = 0;
    for line in file_contents.lines() {
        let split: Vec<_> = line.split_whitespace().collect();
        let x: i32 = split[1].parse().unwrap();
        match split[0] {
            "forward" => {
                hor_pos += x;
                depth += aim * x;
            }
            "down" => aim += x,
            "up" => aim -= x,
            _ => unreachable!(),
        }
    }

    println!("Part Two: {}", depth * hor_pos);
}
