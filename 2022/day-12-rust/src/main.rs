use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("test.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
}

fn part_one(file_contents: &str) {
    let maze: Vec<Vec<char>> = file_contents
        .lines()
        .map(|x| x.chars().collect::<Vec<char>>())
        .collect();
    let mut seen: Vec<Vec<bool>> = vec![vec![false; maze[0].len()]; maze.len()];
    let mut path: Vec<Point> = Vec::new();
    let start = get_start(&maze);

    let _found = walk(&maze, start, None, 'E', &mut seen, &mut path);

    println!("Part One: {}", path.len() as i32 - 1);
}

#[derive(Clone, Copy, Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

use Direction::*;
const DIRECTIONS: [Direction; 4] = [Right, Down, Up, Left];

#[derive(Clone, Copy, Debug)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn move_in(&self, dir: Direction) -> Result<Point, ()> {
        let mut point = *self;
        match dir {
            Up if point.y == 0 => return Err(()),
            Up => point.y -= 1,
            Down => point.y += 1,
            Left if point.x == 0 => return Err(()),
            Left => point.x -= 1,
            Right => point.x += 1,
        }

        Ok(point)
    }
}

fn get_start(maze: &[Vec<char>]) -> Point {
    for (i, row) in maze.iter().enumerate() {
        for (j, col) in row.iter().enumerate() {
            if *col == 'S' {
                return Point { x: j, y: i };
            }
        }
    }

    panic!("no start point");
}

fn walk(
    maze: &Vec<Vec<char>>,
    curr: Point,
    prev_char: Option<char>,
    end_char: char,
    seen: &mut Vec<Vec<bool>>,
    path: &mut Vec<Point>,
) -> bool {
    // pre
    if curr.x >= maze[0].len() || curr.y >= maze.len() {
        return false;
    }

    let curr_char = maze[curr.y][curr.x];
    if curr_char == end_char {
        path.push(curr);
        return true;
    }

    if seen[curr.y][curr.x] {
        return false;
    }

    // println!("{curr_char} {prev_char:?} {path:?}");

    if let Some(prev) = prev_char {
        let char_diff = curr_char as i8 - prev as i8;
        if !(0..=1).contains(&char_diff) && prev != 'S' {
            return false;
        }
    }

    seen[curr.y][curr.x] = true;
    path.push(curr);

    // recurse
    for dir in DIRECTIONS {
        let next_point = match curr.move_in(dir) {
            Ok(v) => v,
            Err(_) => continue,
        };

        // println!(">> {dir:?}");

        if walk(maze, next_point, Some(curr_char), end_char, seen, path) {
            return true;
        }
    }

    path.pop();

    // post

    false
}
