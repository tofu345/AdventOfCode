use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    part_two(&file_contents);
}

fn create_dataset(line: &str) -> Vec<Vec<i32>> {
    let mut report: Vec<Vec<i32>> = vec![line
        .split_whitespace()
        .map(|v| v.parse().unwrap())
        .collect()];

    while report.last().unwrap().iter().sum::<i32>() != 0 {
        let last = report.last().unwrap();
        let mut arr: Vec<i32> = Vec::new();
        for j in 0..last.len() - 1 {
            arr.push(last[j + 1 as usize] - last[j as usize]);
        }
        report.push(arr);
    }

    report
}

fn part_one(file_contents: &str) {
    let mut sum = 0;
    for line in file_contents.lines() {
        let mut report = create_dataset(line);
        for i in (0..report.len() - 1).rev() {
            let placeholder = report[i + 1].last().unwrap() + report[i].last().unwrap();
            report[i].push(placeholder);
        }

        sum += report[0].last().unwrap();
    }

    println!("Part One: {}", sum);
}

fn part_two(file_contents: &str) {
    let mut sum = 0;
    for line in file_contents.lines() {
        let mut report = create_dataset(line);
        for i in (0..report.len() - 1).rev() {
            let placeholder = report[i][0] - report[i + 1][0];
            report[i].insert(0, placeholder);
        }

        sum += report[0][0];
    }

    println!("Part Two: {}", sum);
}
