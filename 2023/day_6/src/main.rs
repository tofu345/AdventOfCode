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
    let lines = file_contents.split("\n").collect::<Vec<&str>>();
    let times: Vec<u32> = lines[0]
        .strip_prefix("Time:")
        .unwrap()
        .split_ascii_whitespace()
        .map(|v| v.parse::<u32>().unwrap())
        .collect();
    let distances: Vec<u32> = lines[1]
        .strip_prefix("Distance:")
        .unwrap()
        .split_ascii_whitespace()
        .map(|v| v.parse::<u32>().unwrap())
        .collect();

    let mut total_wins: Vec<u32> = Vec::new();
    for (&r_time, &r_distance) in times.iter().zip(distances.iter()) {
        let wins = (0..r_time)
            .into_iter()
            .filter(|t| (r_time - t) * t > r_distance)
            .count();

        if wins != 0 {
            total_wins.push(wins as u32);
        }
    }

    println!("Part One: {}", total_wins.iter().product::<u32>());
}

fn part_two(file_contents: &str) {
    let lines = file_contents.split("\n").collect::<Vec<&str>>();
    let parse = |data: &str, prefix: &str| -> u64 {
        data.strip_prefix(prefix)
            .unwrap()
            .split_ascii_whitespace()
            .fold(String::new(), |acc, v| acc + v)
            .parse::<u64>()
            .unwrap()
    };

    let time = parse(lines[0], "Time:");
    let distance = parse(lines[1], "Distance:");
    let wins = (0..time)
        .into_iter()
        .filter(|t| (time - t) * t > distance)
        .count();

    println!("Part Two: {wins}");
}
