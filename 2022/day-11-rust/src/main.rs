use std::process;

fn main() {
    let filename = "input copy.txt";

    if let Err(e) = day_11_rust::part_one(filename) {
        eprintln!("Error: {}", e);
        process::exit(1);
    }

    // if let Err(e) = day_11_rust::part_two(filename) {
    //     eprintln!("Error: {}", e);
    //     process::exit(1);
    // }
}
