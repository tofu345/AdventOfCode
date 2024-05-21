use lazy_static::lazy_static;
use std::{collections::HashMap, fs};

lazy_static! {
    static ref PIPE_MAP: HashMap<char, Vec<Direction>> = HashMap::from([
        ('|', vec![Up, Down]),
        ('-', vec![Left, Right]),
        ('L', vec![Up, Right]),
        ('J', vec![Up, Left]),
        ('7', vec![Down, Left]),
        ('F', vec![Down, Right]),
        ('S', DIRS.to_vec()),
    ]);
}

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    // part_two(&file_contents);
}

#[derive(Debug, Clone, Copy)]
struct Point {
    x: usize,
    y: usize,
}

fn find_start(pipes: &Vec<Vec<char>>) -> Point {
    for x in 0..pipes.len() - 1 {
        for y in 0..pipes[0].len() - 1 {
            if pipes[y][x] == 'S' {
                return Point { x, y };
            }
        }
    }

    panic!("no start point found")
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}
use Direction::*;
const DIRS: [Direction; 4] = [Up, Down, Left, Right];

fn get_next_point(pipes: &Vec<Vec<char>>, mut curr_point: Point, dir: Direction) -> Option<Point> {
    match dir {
        Up if curr_point.y == 0 => return None,
        Up => curr_point.y -= 1,
        Down if curr_point.y == pipes[0].len() - 1 => return None,
        Down => curr_point.y += 1,
        Left if curr_point.x == 0 => return None,
        Left => curr_point.x -= 1,
        Right if curr_point.x == pipes.len() - 1 => return None,
        Right => curr_point.x += 1,
    }

    Some(curr_point)
}

fn connected_to(curr_char: char, next_char: char, dir: Direction) -> bool {
    if next_char == '.' {
        return false;
    }

    if !PIPE_MAP.get(&curr_char).unwrap().contains(&dir) {
        return false;
    }

    let next_dirs = PIPE_MAP.get(&next_char).unwrap();
    match dir {
        Up => next_dirs.contains(&Down),
        Down => next_dirs.contains(&Up),
        Left => next_dirs.contains(&Right),
        Right => next_dirs.contains(&Left),
    }
}

fn walk(
    pipes: &Vec<Vec<char>>,
    pipe_loop: &mut Vec<Point>,
    seen: &mut Vec<Vec<bool>>,
    curr_char: char,
    curr_point: Point,
) -> bool {
    // Prob a better way but idk or care rn
    if curr_char == 'S' && pipe_loop.len() > 5 {
        return true;
    }

    if seen[curr_point.y][curr_point.x] {
        return false;
    }

    seen[curr_point.y][curr_point.x] = true;
    pipe_loop.push(curr_point);

    for dir in DIRS {
        let Some(next_point) = get_next_point(pipes, curr_point, dir) else {
            continue;
        };

        let next_char = pipes[next_point.y][next_point.x];
        if !connected_to(curr_char, next_char, dir) {
            continue;
        }

        if walk(pipes, pipe_loop, seen, next_char, next_point) {
            return true;
        }
    }

    false
}

fn part_one(file_contents: &str) {
    let pipes: Vec<Vec<char>> = file_contents
        .lines()
        .map(|v| v.chars().collect::<Vec<char>>())
        .collect();
    let mut pipe_loop: Vec<Point> = Vec::new();
    let start = find_start(&pipes);
    let mut seen: Vec<Vec<bool>> = vec![vec![false; pipes[0].len()]; pipes.len()];

    let _success = walk(
        &pipes,
        &mut pipe_loop,
        &mut seen,
        pipes[start.y][start.x],
        start,
    );

    // println!("success: {:?}", _success);
    // println!("{:#?} {}", pipe_loop, pipe_loop.len());

    let farthest = pipe_loop[pipe_loop.len() / 2];
    println!(
        "Part One: {} ({})",
        pipe_loop.len() / 2,
        pipes[farthest.y][farthest.x]
    )
}

#[allow(unused)]
fn part_two(file_contents: &str) {
    todo!()
}
