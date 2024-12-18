use std::fs;

const DIRECTIONS: [[i32; 2]; 8] = [
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

    let data = parse(&file_contents);
    part_one(&data);
    part_two(&data);
}

fn parse(file_contents: &str) -> Vec<Vec<char>> {
    file_contents
        .lines()
        .map(|v| v.chars().collect::<Vec<char>>())
        .collect()
}

#[allow(unused)]
fn print_vec(data: &[Vec<char>]) {
    for row in data {
        for &ch in row {
            print!("{ch}");
        }
        println!();
    }
    println!();
}

fn on_neighours(data: &[Vec<char>], idx: (usize, usize)) -> usize {
    let mut neighours = Vec::with_capacity(8);
    let len_y = data.len() as i32;
    let len_x = data[0].len() as i32;
    for &[x, y] in DIRECTIONS.iter() {
        let x = idx.0 as i32 - x;
        let y = idx.1 as i32 - y;
        if x >= 0 && x < len_x && y >= 0 && y < len_y {
            neighours.push(data[y as usize][x as usize]);
        }
    }

    neighours.iter().filter(|&&v| v == '#').count()
}

fn part_one(data: &[Vec<char>]) {
    let mut prev_state = data.to_owned();
    let mut current_state = vec![vec!['\0'; prev_state[0].len()]; prev_state.len()];

    for _ in 0..100 {
        for y in 0..prev_state.len() {
            for x in 0..prev_state[0].len() {
                let ch = prev_state[y][x];
                let on_neighours = on_neighours(&prev_state, (x, y));

                current_state[y][x] = match ch {
                    '.' if on_neighours == 3 => '#',
                    '#' if !(on_neighours == 2 || on_neighours == 3) => '.',
                    _ => ch,
                };
            }
        }

        prev_state = current_state;
        current_state = vec![vec!['\0'; prev_state[0].len()]; prev_state.len()];
    }

    let num_on = prev_state.into_iter().fold(0, |acc, v| {
        acc + v.into_iter().filter(|&v| v == '#').count()
    });
    println!("Part One: {num_on}");
}

fn part_two(data: &[Vec<char>]) {
    let mut prev_state = data.to_owned();
    let mut current_state = vec![vec!['\0'; prev_state[0].len()]; prev_state.len()];
    let len = prev_state.len();
    let edges: [[usize; 2]; 4] = [[0, 0], [0, len - 1], [len - 1, 0], [len - 1, len - 1]];
    edges.iter().for_each(|&[x, y]| prev_state[x][y] = '#');

    for _ in 0..100 {
        for y in 0..prev_state.len() {
            for x in 0..prev_state[0].len() {
                let ch = prev_state[y][x];
                let on_neighours = on_neighours(&prev_state, (x, y));
                if edges
                    .iter()
                    .any(|&[edge_x, edge_y]| edge_x == x && edge_y == y)
                {
                    current_state[y][x] = ch;
                    continue;
                }

                current_state[y][x] = match ch {
                    '.' if on_neighours == 3 => '#',
                    '#' if !(on_neighours == 2 || on_neighours == 3) => '.',
                    _ => ch,
                };
            }
        }

        prev_state = current_state;
        current_state = vec![vec!['\0'; len]; len];
    }

    let num_on = prev_state.into_iter().fold(0, |acc, v| {
        acc + v.into_iter().filter(|&v| v == '#').count()
    });
    println!("Part Two: {num_on}");
}
