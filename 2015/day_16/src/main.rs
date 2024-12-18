use regex::Regex;
use std::{cmp::Ordering, collections::HashMap, fs};

type Aunts<'a> = Vec<(u32, HashMap<&'a str, u32>)>;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    let re = Regex::new(r"Sue (?<id>[0-9]*): (?<things>.*)").unwrap();
    let aunts: Vec<(u32, HashMap<&str, u32>)> = re
        .captures_iter(&file_contents)
        .map(|v| {
            let id: u32 = v.name("id").unwrap().as_str().parse().unwrap();
            let map = HashMap::from_iter(v.name("things").unwrap().as_str().split(", ").map(|v| {
                let splits: Vec<&str> = v.split(": ").collect();
                (splits[0], splits[1].parse().unwrap())
            }));
            (id, map)
        })
        .collect();
    let target_aunt = HashMap::from([
        ("children", 3),
        ("cat", 7),
        ("samoyeds", 2),
        ("pomeranians", 3),
        ("akitas", 0),
        ("vizslas", 0),
        ("goldfish", 5),
        ("trees", 3),
        ("cars", 2),
        ("perfumes", 1),
    ]);

    part_one(&aunts, &target_aunt);
    part_two(&aunts, &target_aunt);
}

fn part_one(aunts: &Aunts, target: &HashMap<&str, u32>) {
    for (id, data) in aunts {
        let is_correct_aunt = data.iter().all(|(&k, &v)| match target.get(k) {
            Some(target_v) => *target_v == v,
            None => false,
        });
        if is_correct_aunt {
            println!("Part One: {id}");
            return;
        }
    }

    println!("aunt not found");
}

fn part_two(aunts: &Aunts, target: &HashMap<&str, u32>) {
    for (id, data) in aunts {
        let is_correct_aunt = data.iter().all(|(&key, &value)| {
            let cmp = match key {
                "cats" | "trees" => Ordering::Greater,
                "pomeranians" | "goldfish" => Ordering::Less,
                _ => Ordering::Equal,
            };
            let target_v = target.get(key);
            target_v.is_some() && value.cmp(target_v.unwrap()) == cmp
        });

        if is_correct_aunt {
            println!("Part Two: {id}");
            return;
        }
    }

    println!("aunt not found");
}
