use std::fs;

fn main() {
    let filename = "input.txt";
    let file_contents = match fs::read_to_string(filename) {
        Ok(v) => v,
        Err(_) => panic!("could not read file"),
    };

    if let Err(e) = run(&file_contents) {
        panic!("Error {}", e);
    }
}

fn run(file_contents: &str) -> Result<(), &'static str> {
    let mut floor = 0;
    for (i, c) in file_contents.chars().enumerate() {
        floor += if c == '(' { 1 } else { -1 };
        if floor == -1 {
            println!("Santa enters the basement at position {}", i + 1);
            return Ok(());
        }
    }

    println!("Floor: {floor}");

    Ok(())
}
