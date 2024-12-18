use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    // part_two(&file_contents);
}

fn part_one(file_contents: &str) {
    todo!()
}

fn part_two(file_contents: &str) {
    todo!()
}
