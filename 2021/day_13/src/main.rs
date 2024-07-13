use std::{collections::HashSet, fs, io, usize};

use regex::Regex;

fn main() -> io::Result<()> {
    let file_contents = fs::read_to_string("input.txt")?;

    let (dots, folds) = parse(&file_contents);
    part_one(&dots, &folds);
    part_two(&dots, &folds);
    Ok(())
}

type Dots = HashSet<(usize, usize)>;
type Folds<'a> = Vec<(&'a str, usize)>;

fn parse(file_contents: &str) -> (Dots, Folds) {
    let dots = Regex::new(r"(?<x>.*),(?<y>.*)")
        .unwrap()
        .captures_iter(
            &file_contents
                .lines()
                .take_while(|v| !v.is_empty())
                .collect::<Vec<&str>>()
                .join("\n"),
        )
        .map(|c| c.extract())
        .map(|(_, [x, y])| (x.parse::<usize>().unwrap(), y.parse::<usize>().unwrap()))
        .collect();
    let (_, folds_str) = file_contents.split_once("\n\n").unwrap();
    let folds = Regex::new(r"fold along (?<axis>.)=(?<val>[0-9]*)")
        .unwrap()
        .captures_iter(folds_str)
        .map(|c| c.extract())
        .map(|(_, [axis, val])| (axis, val.parse::<usize>().unwrap()))
        .collect();
    (dots, folds)
}

fn fold_y(dots: &Dots, mid_point: usize) -> Dots {
    dots.iter()
        .map(|&(x, mut y)| {
            if y >= mid_point {
                y = mid_point - (y - mid_point);
            }
            (x, y)
        })
        .collect()
}

fn fold_x(dots: &Dots, mid_point: usize) -> Dots {
    dots.iter()
        .map(|&(mut x, y)| {
            if x >= mid_point {
                x = mid_point - (x - mid_point);
            }
            (x, y)
        })
        .collect()
}

fn part_one(dots: &Dots, folds: &Folds) {
    let &(axis, val) = &folds[0];
    let new_dots = match axis {
        "x" => fold_x(dots, val),
        "y" => fold_y(dots, val),
        _ => unreachable!(),
    };
    println!("Part One: {}", new_dots.len());
}

fn part_two(dots: &Dots, folds: &Folds) {
    let final_dots = folds.iter().fold(dots.clone(), |acc, v| {
        let &(axis, val) = v;
        match axis {
            "x" => fold_x(&acc, val),
            "y" => fold_y(&acc, val),
            _ => unreachable!(),
        }
    });
    let max_x = final_dots.iter().map(|(x, _)| *x).max().unwrap();
    let max_y = final_dots.iter().map(|(_, y)| *y).max().unwrap();
    let mut graph = vec![vec!['.'; max_x + 1]; max_y + 1];
    for &(x, y) in final_dots.iter() {
        graph[y][x] = '#';
    }
    for row in graph {
        for val in row {
            print!("{val}");
        }
        println!();
    }
}
