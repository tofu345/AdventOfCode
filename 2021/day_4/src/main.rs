use std::{fs, u32};

fn main() {
    let file_contents = match fs::read_to_string("test.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    let (boards, draws) = parse(&file_contents);

    part_one(&draws, &boards);
    // part_two(&draws, &boards);
}

fn part_one(draws: &Draws, boards: &Boards) {
    for i in 1..draws.len() {
        for board in boards {
            if is_bingo(&draws[..=i], board) {
                let sum: u32 = board
                    .iter()
                    .flatten()
                    .filter(|&v| !draws[..=i].contains(v))
                    .sum();

                println!("Part One: {}", sum * draws[i]);
                return;
            }
        }
    }
}

#[allow(unused)]
fn part_two(draws: &Draws, boards: &Boards) {
    todo!()
}

type Board = Vec<Vec<u32>>;
type Boards = Vec<Board>;
type Draws = Vec<u32>;

fn parse(file_contents: &str) -> (Boards, Draws) {
    let mut data = file_contents.lines();
    let draws: Draws = data
        .next()
        .unwrap()
        .split(',')
        .map(|v| v.parse().unwrap())
        .collect();
    let mut boards: Boards = Vec::new();
    for line in data {
        if line.is_empty() {
            boards.push(Vec::new());
            continue;
        }

        let board = line
            .split_ascii_whitespace()
            .map(|v| v.parse().unwrap())
            .collect();
        boards.last_mut().unwrap().push(board);
    }

    (boards, draws)
}

#[allow(clippy::needless_range_loop)]
fn is_bingo(draws: &[u32], board: &Board) -> bool {
    'outer: for y in 0..board.len() {
        for x in 0..board[0].len() {
            if !draws.contains(&board[y][x]) {
                continue 'outer;
            }
        }
        return true;
    }

    'outer: for x in 0..board[0].len() {
        for y in 0..board.len() {
            if !draws.contains(&board[y][x]) {
                continue 'outer;
            }
        }
        return true;
    }

    false
}
