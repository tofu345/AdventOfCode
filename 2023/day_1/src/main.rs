use std::fs;

// Probably should have used a hashmap but it works so we good :>
const DIGITS: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    part_two(&file_contents)
}

fn part_one(file_contents: &str) {
    let mut total_values: u32 = 0;
    for line in file_contents.lines() {
        let first_num = line.chars().find(|&x| x.is_digit(10)).unwrap();
        let last_num = line.chars().rev().find(|&x| x.is_digit(10)).unwrap();
        let num: u32 = format!("{first_num}{last_num}").parse().unwrap();
        total_values += num;
    }

    println!("Part One: {total_values}");
}

fn part_two(file_contents: &str) {
    let mut total_values: u32 = 0;
    for line in file_contents.lines() {
        let digits = get_digits(line);
        let first_num = digits[0];
        let last_num = digits[digits.len() - 1];
        let num: u32 = format!("{first_num}{last_num}").parse().unwrap();
        total_values += num;
    }

    println!("Part Two: {total_values}");
}

fn get_digits(line: &str) -> Vec<u32> {
    let mut digits: Vec<u32> = Vec::new();
    for (i, ch) in line.chars().enumerate() {
        if ch.is_digit(10) {
            digits.push(ch.to_string().parse::<u32>().unwrap());
            continue;
        }

        for digit in DIGITS {
            if ch == digit.chars().next().unwrap() {
                let index = line[i..].find(digit).map(|x| x + i);
                if let Some(v) = index {
                    if v == i {
                        digits.push(my_atoi(digit));
                    }
                }
            }
        }
    }

    digits
}

fn my_atoi(digit: &str) -> u32 {
    // 0, 1, 2, 3, ...
    if digit.len() == 1 && digit.chars().next().unwrap().is_digit(10) {
        return digit.parse().unwrap();
    }

    // one, two, three, ...
    DIGITS
        .iter()
        .position(|&x| x == digit)
        .expect("{digit} could not be converted") as u32
        + 1
}
