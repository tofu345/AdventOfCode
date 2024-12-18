use regex::Regex;
use std::{char, collections::HashMap, fs, io};

fn main() -> io::Result<()> {
    let file_contents = fs::read_to_string("test.txt")?;
    let (polymer, pairs) = parse(&file_contents);

    part_one(polymer, &pairs);
    // part_two(&file_contents);
    Ok(())
}

type Pairs = HashMap<String, char>;

fn parse(file_contents: &str) -> (&str, Pairs) {
    let (polymer, pairs) = file_contents.split_once("\n\n").unwrap();
    let map = Regex::new(r"(?<from>.*) -> (?<to>.)")
        .unwrap()
        .captures_iter(pairs)
        .map(|c| c.extract())
        .map(|(_, [from, to])| {
            let to = to.chars().next().unwrap();
            (from.to_string(), to)
        })
        .collect();
    (polymer, map)
}

fn recurse(pairs: &Pairs, polymer: String, steps_left: u32) -> String {
    let mut new = String::new();
    new.push(polymer.chars().next().unwrap());
    let mut prev: Option<char> = None;
    for ch in polymer.chars() {
        if let Some(prv) = prev {
            new.push(ch);
            let mut string = prv.to_string();
            string.push(ch);
            new.push(*pairs.get(&string).unwrap());
            prev = None;
            continue;
        }
        prev = Some(ch);
    }

    if steps_left == 0 {
        new
    } else {
        recurse(pairs, new, steps_left - 1)
    }
}

fn part_one(polymer: &str, pairs: &Pairs) {
    let new_polymer = recurse(pairs, polymer.to_string(), 1);
    let mut nums: HashMap<char, u32> = HashMap::new();
    new_polymer.chars().for_each(|v| {
        nums.entry(v).and_modify(|v| *v += 1).or_insert(1);
    });

    println!(
        "{} {:?}",
        nums.values().max().unwrap() - nums.values().min().unwrap(),
        new_polymer
    );
}

fn part_two(file_contents: &str) {
    todo!()
}
