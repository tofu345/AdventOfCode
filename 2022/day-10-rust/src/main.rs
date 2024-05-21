use std::process;

fn main() {
    let filename = "input.txt";

    if let Err(e) = day_10_rust::part_one(filename) {
        eprintln!("Error: {}", e);
        process::exit(1);
    }

    println!("-----------");

    if let Err(e) = day_10_rust::part_two(filename) {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}
