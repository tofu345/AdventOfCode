use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("test.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    // part_two(&file_contents);
}

fn part_one(file_contents: &str) {
    let mut total = 0;

    let data: Vec<&str> = file_contents.split_whitespace().collect();
    let row = data[0];
    let springs: Vec<i32> = data[1]
        .split(',')
        .map(|v| v.parse::<i32>().unwrap())
        .collect();

    total += count_arrangements(row, springs);
    println!("{}", total);
    // println!("");
}

fn count_arrangements(row: &str, springs: Vec<i32>) -> i32 {
    0
}

#[allow(unused)]
fn part_two(file_contents: &str) {
    todo!()
}
