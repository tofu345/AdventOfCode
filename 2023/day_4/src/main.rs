use std::{collections::HashMap, fs};

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    part_two(&file_contents);
}

fn calc_points(nums: u32) -> u32 {
    match nums {
        0 => 0,
        1 => 1,
        _ => 2u32.pow(nums - 1),
    }
}

fn part_one(file_contents: &str) {
    let mut total_points = 0;

    for line in file_contents.lines() {
        let splits = line
            .split(": ")
            .last()
            .map(|v| v.split(" | ").collect::<Vec<&str>>())
            .unwrap();
        let winning_nums: Vec<&str> = splits[0].split_ascii_whitespace().collect();
        let ours: Vec<&str> = splits[1].split_ascii_whitespace().collect();
        let our_winning = ours
            .iter()
            .filter(|x| winning_nums.contains(x))
            .collect::<Vec<&&str>>()
            .len();
        total_points += calc_points(our_winning as u32);
    }

    println!("Part One: {total_points}");
}

fn part_two(file_contents: &str) {
    let mut instances: HashMap<u32, u32> = HashMap::new();

    for line in file_contents.lines() {
        let splits: Vec<&str> = line.split(": ").collect();
        let card_id: u32 = splits[0]
            .strip_prefix("Card ")
            .unwrap()
            .trim()
            .parse()
            .unwrap();

        instances
            .entry(card_id)
            .and_modify(|v| *v += 1)
            .or_insert(1);

        let data: Vec<&str> = splits[1].split(" | ").collect();
        let winning_nums: Vec<&str> = data[0].split_ascii_whitespace().collect();
        let ours: Vec<&str> = data[1].split_ascii_whitespace().collect();
        let our_winning = ours
            .iter()
            .filter(|x| winning_nums.contains(x))
            .collect::<Vec<&&str>>()
            .len() as u32;

        let curr = *instances.get(&card_id).unwrap();
        for _ in 0..curr {
            for i in card_id + 1..=card_id + our_winning {
                instances.entry(i).and_modify(|v| *v += 1).or_insert(1);
            }
        }
    }

    println!("Part Two: {}", instances.values().sum::<u32>());
}
