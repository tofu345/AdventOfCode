use core::cmp::{max, min};
use regex::Regex;
use std::{cmp::Ordering, fs, usize};

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    let lines = parse(&file_contents);

    let mut vents = part_one(lines.to_vec());
    part_two(lines, &mut vents);
}

type VentCoords = Vec<((usize, usize), (usize, usize))>;

fn part_one(coords: VentCoords) -> Vec<Vec<u32>> {
    let mut vents = vec![vec![0u32; 1000]; 1000];
    for ((x1, y1), (x2, y2)) in coords {
        if x1 == x2 {
            (min(y1, y2)..=max(y1, y2)).for_each(|y| {
                vents[y][x1] += 1;
            });
        } else if y1 == y2 {
            for x in min(x1, x2)..=max(x1, x2) {
                vents[y1][x] += 1;
            }
        }
    }

    let overlaps = vents.iter().flatten().filter(|&&v| v >= 2).count();
    println!("Part One: {overlaps}");
    vents
}

fn part_two(coords: VentCoords, vents: &mut [Vec<u32>]) {
    for ((x1, y1), (x2, y2)) in coords {
        let num = max(y1, y2) - min(y1, y2);
        let den = max(x1, x2) - min(x1, x2);
        if den == 0 || (num / den) != 1 {
            continue;
        }

        let x_range: Vec<usize> = match x1.cmp(&x2) {
            Ordering::Less => (x1..=x2).collect(),
            _ => (x2..=x1).rev().collect(),
        };
        let y_range: Vec<usize> = match y1.cmp(&y2) {
            Ordering::Less => (y1..=y2).collect(),
            _ => (y2..=y1).rev().collect(),
        };

        x_range
            .into_iter()
            .zip(y_range.into_iter())
            .for_each(|(x, y)| {
                vents[y][x] += 1;
            });
    }

    let overlaps = vents.iter().flatten().filter(|&&v| v >= 2).count();
    println!("Part Two: {overlaps}");
}

#[allow(unused)]
fn print_vents(vents: &[Vec<u32>]) {
    for row in vents {
        for col in row {
            print!("{col}");
        }
        println!();
    }
    println!();
}

fn parse(file_contents: &str) -> VentCoords {
    Regex::new(r"(?<x1>[0-9]*),(?<y1>[0-9]*) -> (?<x2>[0-9]*),(?<y2>[0-9]*)")
        .unwrap()
        .captures_iter(file_contents)
        .map(|c| c.extract())
        .map(|(_, [x1, y1, x2, y2])| {
            (
                (x1.parse().unwrap(), y1.parse().unwrap()),
                (x2.parse().unwrap(), y2.parse().unwrap()),
            )
        })
        .collect()
}
