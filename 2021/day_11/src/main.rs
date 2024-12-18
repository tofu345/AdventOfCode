use std::{fs, io};

fn main() -> io::Result<()> {
    let file_contents = fs::read_to_string("test.txt")?;
    let data = parse(&file_contents);

    part_one(data.clone());
    // part_two(&file_contents);
    Ok(())
}

fn parse(file_contents: &str) -> Vec<Vec<usize>> {
    file_contents
        .lines()
        .map(|v| {
            v.chars()
                .map(|v| v.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

fn part_one(data: Vec<Vec<usize>>) {
    for _ in 0..100 {
    }
}

fn part_two(file_contents: &str) {
    todo!()
}
