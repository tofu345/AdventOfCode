use std::cmp::{max, min};
use std::collections::HashMap;
use std::{fs, usize};

fn main() {
    let file_contents = match fs::read_to_string("test.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    part_two(&file_contents);
}

fn part_one(file_contents: &str) {
    let data: Vec<Vec<char>> = parse_data(file_contents);
    let mut galaxies = Vec::new();

    for y in 0..data.len() {
        for x in 0..data[0].len() {
            if data[y][x] == '#' {
                galaxies.push((x, y));
            }
        }
    }

    let mut paths = Vec::new();
    let mut seen: HashMap<(usize, usize), Vec<(usize, usize)>> = HashMap::new();

    for (x1, y1) in galaxies.iter().copied() {
        for (x2, y2) in galaxies.iter().copied() {
            if x1 == x2 && y1 == y2 {
                continue;
            }

            if let Some(v) = seen.get(&(x1, y1)) {
                if v.contains(&(x2, y2)) {
                    continue;
                }
            }

            if let Some(v) = seen.get(&(x2, y2)) {
                if v.contains(&(x1, y1)) {
                    continue;
                }
            }

            seen.entry((x1, y1))
                .and_modify(|v: &mut Vec<(usize, usize)>| {
                    v.push((x2, y2));
                })
                .or_insert(vec![(x2, y2)]);

            paths.push(i32::abs(x1 as i32 - x2 as i32) + i32::abs(y1 as i32 - y2 as i32));
        }
    }

    // println!("{:?}", galaxies);
    // display(&data);
    println!("Part One: {:?}", paths.iter().sum::<i32>());
}

fn parse_data(file_contents: &str) -> Vec<Vec<char>> {
    let mut data: Vec<Vec<char>> = file_contents.lines().map(|v| v.chars().collect()).collect();

    let mut skip = false;
    let mut y = 0;
    loop {
        if y == data.len() {
            break;
        }

        if skip {
            y += 1;
            skip = false;
            continue;
        }

        if data[y].iter().find(|&&x| x == '#').is_none() {
            skip = true;
            data.insert(y, data[y].clone());
        }

        y += 1;
    }

    skip = false;
    let mut x = 0;
    loop {
        if x == data[0].len() {
            break;
        }

        if skip {
            x += 1;
            skip = false;
            continue;
        }

        if (0..data.len()).into_iter().all(|y| data[y][x] != '#') {
            skip = true;
            for y in 0..data.len() {
                let v = data[y][x];
                data[y].insert(x, v);
            }
        }

        x += 1;
    }

    data
}

fn display(data: &Vec<Vec<char>>) {
    for y in 0..data.len() {
        for x in 0..data[0].len() {
            print!("{}", data[y][x]);
        }
        println!();
    }
}

fn get_empty_rows(data: &Vec<Vec<char>>) -> Vec<i32> {
    let mut empty_rows = vec![];

    for x in 0..data.len() {
        if data[x].iter().find(|&&x| x == '#').is_none() {
            empty_rows.push(x as i32);
        }
    }

    empty_rows
}

fn get_empty_cols(data: &Vec<Vec<char>>) -> Vec<i32> {
    let mut empty_cols = vec![];

    for x in 0..data[0].len() {
        if (0..data.len()).into_iter().all(|y| data[y][x] != '#') {
            empty_cols.push(x as i32);
        }
    }

    empty_cols
}

fn part_two(file_contents: &str) {
    // good night, good luck remembering the problem future tofs
    //
    // future tofs could not fix the problem :<

    let data: Vec<Vec<char>> = file_contents.lines().map(|v| v.chars().collect()).collect();
    let mut galaxies = Vec::new();

    for y in 0..data.len() {
        for x in 0..data[0].len() {
            if data[y][x] == '#' {
                galaxies.push((x as i32, y as i32));
            }
        }
    }

    let mut paths = Vec::new();
    let mut seen: HashMap<(i32, i32), Vec<(i32, i32)>> = HashMap::new();
    let empty_rows = get_empty_rows(&data);
    let empty_cols = get_empty_cols(&data);

    for (x1, y1) in galaxies.iter().copied() {
        for (x2, y2) in galaxies.iter().copied() {
            if x1 == x2 && y1 == y2 {
                continue;
            }

            if let Some(v) = seen.get(&(x1, y1)) {
                if v.contains(&(x2, y2)) {
                    continue;
                }
            }

            if let Some(v) = seen.get(&(x2, y2)) {
                if v.contains(&(x1, y1)) {
                    continue;
                }
            }

            seen.entry((x1, y1))
                .and_modify(|v: &mut Vec<(i32, i32)>| v.push((x2, y2)))
                .or_insert(vec![(x2, y2)]);

            let mut empty_rows_btwn = empty_rows
                .iter()
                .filter(|&&x| x < max(x1, x2) && x > min(x1, x2))
                .count() as i32;

            let mut empty_cols_btwn = empty_cols
                .iter()
                .filter(|&&y| y < max(y1, y2) && y > min(y1, y2))
                .count() as i32;

            if empty_rows_btwn > 0 {
                empty_rows_btwn = empty_rows_btwn * 2 - empty_rows_btwn;
            }
            if empty_cols_btwn > 0 {
                empty_cols_btwn = empty_cols_btwn * 2 - empty_cols_btwn;
            }

            println!(
                "{:?} {:?} {:?} {:?} = {:?}",
                (x1, y1),
                (x2, y2),
                empty_rows_btwn,
                empty_cols_btwn,
                ((max(x1, x2) + empty_rows_btwn) - min(x1, x2))
                    + ((max(y1, y2) + empty_cols_btwn) - min(y1, y2)),
            );

            // paths.push(
            //     ((max(x1, x2) + empty_rows_btwn) - min(x1, x2))
            //         + ((max(y1, y2) + empty_cols_btwn) - min(y1, y2)),
            // );
            paths.push(
                ((max(x1, x2) + empty_rows_btwn) - min(x1, x2))
                    + ((max(y1, y2) + empty_cols_btwn) - min(y1, y2)),
            );

            // + (i32::abs(y1 as i32 - y2 as i32) + (empty_cols_btwn.len() * 10) as i32),
        }
    }

    println!("{}", paths[4]);

    // println!("{:?}", empty_cols);
    // display(&data);
    println!("Part Two: {:?}", paths.iter().sum::<i32>());
}
