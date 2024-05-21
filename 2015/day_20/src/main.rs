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
    let target: i32 = file_contents.trim().parse().unwrap();
    for i in 1.. {
        let num_presents = (1..=i).filter(|v| i % v == 0).sum::<i32>() * 10;
        if num_presents >= target {
            println!("Part One: {i}");
            return;
        }
    }
}

fn part_two(file_contents: &str) {
    todo!()
}
