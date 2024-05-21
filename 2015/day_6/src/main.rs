use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(_) => panic!("could not read file"),
    };

    if let Err(e) = part_one(&file_contents) {
        eprintln!("Error: {}", e);
        return;
    }

    if let Err(e) = part_two(&file_contents) {
        eprintln!("Error: {}", e);
    }
}

fn part_one(file_contents: &str) -> Result<(), &'static str> {
    let mut lights = vec![vec![false; 1000]; 1000];

    for orig_line in file_contents.lines() {
        let line: &str;

        if orig_line.starts_with("turn on") {
            line = orig_line.strip_prefix("turn on ").unwrap();
        } else if orig_line.starts_with("turn off") {
            line = orig_line.strip_prefix("turn off ").unwrap();
        } else {
            line = orig_line.strip_prefix("toggle ").unwrap();
        }

        let ((start_row, start_col), (end_row, end_col)) = parse_line(line);

        for row in start_row..=end_row {
            for col in start_col..=end_col {
                if orig_line.starts_with("turn on") {
                    lights[row][col] = true;
                } else if orig_line.starts_with("turn off") {
                    lights[row][col] = false;
                } else {
                    lights[row][col] = !lights[row][col];
                }
            }
        }
    }

    let mut lit_lights = 0;
    for row in 0..lights.len() {
        for col in 0..lights[0].len() {
            if lights[row][col] {
                lit_lights += 1;
            }
        }
    }

    println!("Lit lights (part one) {}", lit_lights);

    Ok(())
}

fn part_two(file_contents: &str) -> Result<(), &'static str> {
    let mut lights = vec![vec![0u32; 1000]; 1000];

    for orig_line in file_contents.lines() {
        let line: &str;

        if orig_line.starts_with("turn on") {
            line = orig_line.strip_prefix("turn on ").unwrap();
        } else if orig_line.starts_with("turn off") {
            line = orig_line.strip_prefix("turn off ").unwrap();
        } else {
            line = orig_line.strip_prefix("toggle ").unwrap();
        }

        let ((start_row, start_col), (end_row, end_col)) = parse_line(line);

        for row in start_row..=end_row {
            for col in start_col..=end_col {
                if orig_line.starts_with("turn on") {
                    lights[row][col] += 1;
                } else if orig_line.starts_with("turn off") {
                    if lights[row][col] > 0 {
                        lights[row][col] -= 1;
                    }
                } else if orig_line.starts_with("toggle") {
                    lights[row][col] += 2;
                } else {
                    return Err("invalid input data");
                }
            }
        }
    }

    let mut total_brightness: u128 = 0;
    for row in 0..lights.len() {
        for col in 0..lights[0].len() {
            total_brightness += lights[row][col] as u128;
        }
    }

    println!("Total Brightness (part two) {}", total_brightness);

    Ok(())
}

fn parse_line(line: &str) -> ((usize, usize), (usize, usize)) {
    let positions: Vec<&str> = line.split(" through ").collect();
    let start_pos: Vec<&str> = positions[0].split(",").collect();
    let end_pos: Vec<&str> = positions[1].split(",").collect();
    let (start_row, start_col) = (
        start_pos[0].parse::<usize>().unwrap(),
        start_pos[1].parse::<usize>().unwrap(),
    );
    let (end_row, end_col) = (
        end_pos[0].parse::<usize>().unwrap(),
        end_pos[1].parse::<usize>().unwrap(),
    );

    ((start_row, start_col), (end_row, end_col))
}
