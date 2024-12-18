use std::{collections::HashMap, fs};

const POSITIONS: [[isize; 2]; 8] = [
    [-1, -1],
    [-1, 0],
    [-1, 1],
    [0, 1],
    [1, 1],
    [1, 0],
    [1, -1],
    [0, -1],
];

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    part_two(&file_contents);
}

fn part_one(file_contents: &str) {
    let data: Vec<Vec<char>> = file_contents
        .lines()
        .map(|v| v.chars().collect::<Vec<char>>())
        .collect();
    let num_cols = data[0].len();
    let mut sum = 0;

    for y in 0..data.len() {
        let mut skip = 0;

        for x in 0..num_cols {
            if skip > 0 {
                skip -= 1;
                continue;
            }

            let ch = data[y][x];
            if ch.is_digit(10) {
                let end = data[y][x..num_cols]
                    .iter()
                    .position(|x| !x.is_digit(10))
                    .map(|j| j + x)
                    .unwrap_or(num_cols);
                let num = String::from_iter(data[y][x..end].to_vec());

                for i in x..end {
                    if adjacent_to_symbol(&data, i, y) {
                        // println!("{num}");
                        sum += num.parse::<i32>().unwrap();
                        break;
                    }
                }

                skip = end - x;
            }
        }
    }

    println!("Part One: {sum}");
}

fn part_two(file_contents: &str) {
    let data: Vec<Vec<char>> = file_contents
        .lines()
        .map(|v| v.chars().collect::<Vec<char>>())
        .collect();
    let num_cols = data[0].len();
    let mut stars: HashMap<(usize, usize), Vec<String>> = HashMap::new();

    for y in 0..data.len() {
        let mut skip = 0;

        for x in 0..num_cols {
            if skip > 0 {
                skip -= 1;
                continue;
            }

            let ch = data[y][x];
            if ch.is_digit(10) {
                let end = data[y][x..num_cols]
                    .iter()
                    .position(|x| !x.is_digit(10))
                    .map(|j| j + x)
                    .unwrap_or(num_cols);
                let num = String::from_iter(data[y][x..end].to_vec());

                for i in x..end {
                    let (adjacent, pos) = adjacent_to_star(&data, i, y);
                    if adjacent {
                        let (cx, cy) = pos.unwrap();
                        stars
                            .entry((cx, cy))
                            .and_modify(|v| v.push(num.clone()))
                            .or_insert(vec![num]);
                        break;
                    }
                }

                skip = end - x;
            }
        }
    }

    let mut sum = 0;
    for (_, v) in stars.iter() {
        if v.len() == 2 {
            sum += v[0].parse::<i32>().unwrap() * v[1].parse::<i32>().unwrap();
        }
    }

    println!("Part One: {sum}");
}

fn adjacent_to_star(data: &Vec<Vec<char>>, x: usize, y: usize) -> (bool, Option<(usize, usize)>) {
    for pos in POSITIONS {
        let cx = x as isize + pos[0];
        let cy = y as isize + pos[1];

        if cx >= 0 && cy >= 0 {
            let cx = cx as usize;
            let cy = cy as usize;

            if cx < data[0].len() && cy < data.len() {
                // println!(">{}, {pos:?}", data[cy][cx]);

                if is_star(data[cy][cx]) {
                    return (true, Some((cx, cy)));
                }
            }
        }
    }

    (false, None)
}

fn is_star(ch: char) -> bool {
    is_symbol(ch) && ch == '*'
}

fn adjacent_to_symbol(data: &Vec<Vec<char>>, x: usize, y: usize) -> bool {
    for pos in POSITIONS {
        let cx = x as isize + pos[0];
        let cy = y as isize + pos[1];

        if cx >= 0 && cy >= 0 {
            let cx = cx as usize;
            let cy = cy as usize;

            if cx < data[0].len() && cy < data.len() {
                // println!(">{}, {pos:?}", data[cy][cx]);

                if is_symbol(data[cy][cx]) {
                    return true;
                }
            }
        }
    }

    false
}

fn is_symbol(ch: char) -> bool {
    !ch.is_digit(10) && ch != '.'
}
