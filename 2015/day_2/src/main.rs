use std::{cmp, fs};

fn main() {
    let filename = "input.txt";

    if let Err(e) = part_one(filename) {
        eprintln!("Error: {}", e);
    }

    if let Err(e) = part_two(filename) {
        eprintln!("Error: {}", e);
    }
}

fn part_one(filename: &'static str) -> Result<(), &'static str> {
    let file_contents = match fs::read_to_string(filename) {
        Ok(v) => v,
        Err(_) => return Err("could not read file"),
    };

    let mut total = 0;
    for line in file_contents.lines() {
        let line: Vec<&str> = line.split('x').collect();
        if line.len() < 3 {
            return Err("invalid input data");
        }

        let l: i32 = line[0].parse().unwrap();
        let w: i32 = line[1].parse().unwrap();
        let h: i32 = line[2].parse().unwrap();
        let paper_amount = 2 * l * w + 2 * w * h + 2 * h * l;
        let slack = cmp::min(cmp::min(l * w, w * h), h * l);
        total += paper_amount + slack;
    }

    println!("Total (part one) {}", total);

    Ok(())
}

fn part_two(filename: &'static str) -> Result<(), &'static str> {
    let file_contents = match fs::read_to_string(filename) {
        Ok(v) => v,
        Err(_) => return Err("could not read file"),
    };

    let mut total = 0;
    for line in file_contents.lines() {
        let line: Vec<&str> = line.split('x').collect();
        if line.len() < 3 {
            return Err("invalid input data");
        }

        let l: i32 = line[0].parse().unwrap();
        let w: i32 = line[1].parse().unwrap();
        let h: i32 = line[2].parse().unwrap();
        let ribbon = 2 * cmp::min(cmp::min(l + w, w + h), h + l);
        let bow = l * w * h;
        total += ribbon + bow;
    }

    println!("Total (part two): {}", total);

    Ok(())
}
