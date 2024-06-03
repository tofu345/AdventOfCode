use std::{fs, io};

fn main() -> io::Result<()> {
    let file_contents = fs::read_to_string("test.txt")?;

    part_one(&file_contents);
    // part_two(&file_contents);
    Ok(())
}

fn part_one(file_contents: &str) {
    todo!()
}

fn part_two(file_contents: &str) {
    todo!()
}
