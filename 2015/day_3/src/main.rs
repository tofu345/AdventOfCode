use std::fs;

fn main() {
    let filename = "input.txt";

    if let Err(e) = part_one(filename) {
        eprintln!("Error: {}", e);
        return;
    }

    if let Err(e) = part_two(filename) {
        eprintln!("Error: {}", e);
        return;
    }
}

fn part_one(filename: &'static str) -> Result<(), &'static str> {
    let file_contents = match fs::read_to_string(filename) {
        Ok(v) => v,
        Err(_) => return Err("could not read file"),
    };

    let mut houses = [[false; 500]; 500];
    let mut x = houses.len() / 2;
    let mut y = houses[0].len() / 2;
    let mut delivered = 0;

    for c in file_contents.chars() {
        if !houses[x][y] {
            delivered += 1;
            houses[x][y] = true;
        };

        match c {
            '>' => x += 1,
            '<' => x -= 1,
            '^' => y -= 1,
            'v' => y += 1,
            _ => unreachable!(),
        };
    }

    println!("Part One: {}", delivered);

    Ok(())
}

fn part_two(filename: &'static str) -> Result<(), &'static str> {
    let file_contents = match fs::read_to_string(filename) {
        Ok(v) => v,
        Err(_) => return Err("could not read file"),
    };

    let mut houses = [[0i32; 500]; 500];
    let mut santa = (houses.len() / 2, houses[0].len() / 2);
    let mut robo_santa = santa.clone();
    let mut at_least_one = 1;
    houses[santa.0][santa.1] = 1;

    for (i, c) in file_contents.chars().enumerate() {
        let pos = if (i + 1) % 2 == 0 {
            &mut robo_santa
        } else {
            &mut santa
        };

        match c {
            '>' => pos.0 += 1,
            '<' => pos.0 -= 1,
            '^' => pos.1 -= 1,
            'v' => pos.1 += 1,
            _ => unreachable!(),
        };

        houses[pos.0][pos.1] += 1;
        if houses[pos.0][pos.1] == 1 {
            at_least_one += 1;
        }
    }

    println!("Part two: {}", at_least_one);

    Ok(())
}
