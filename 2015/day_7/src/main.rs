use regex::Regex;
use std::{collections::HashMap, fs};

fn main() {
    let file_contents = match fs::read_to_string("test.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents)
}

type Wires = HashMap<String, u16>;

#[derive(Debug)]
struct Instruction<'a> {
    lhs: &'a str,
    rhs: &'a str,
}

fn part_one(file_contents: &str) {
    let mut wires: Wires = HashMap::new();
    let instructions = parse(file_contents);

    for ins in instructions.iter() {
        let lhs = eval(ins.lhs, &mut wires);
        wires.insert(ins.rhs.to_owned(), lhs);
    }

    println!("{:#?}", wires);
    println!("part one: {:?}", wires.get("a"));
}

fn eval(lhs: &str, wires: &mut Wires) -> u16 {
    if !lhs.contains(' ') {
        if lhs.chars().all(char::is_numeric) {
            return lhs.parse().unwrap();
        }

        return *wires.entry(lhs.to_owned()).or_insert(0);
    }

    let args: Vec<&str> = lhs.split_whitespace().collect();
    if args[0] == "NOT" {
        return !eval(args[1], wires);
    }

    let x = eval(args[0], wires);
    let y = eval(args[2], wires);
    match args[1] {
        "AND" => x & y,
        "OR" => x | y,
        "LSHIFT" => x << y,
        "RSHIFT" => x >> y,
        _ => unreachable!(),
    }
}

fn parse(lines: &str) -> Vec<Instruction> {
    let re = Regex::new(r"(?<lhs>.+) -> (?<rhs>.+)").unwrap();
    re.captures_iter(lines)
        .map(|v| {
            let lhs = v.name("lhs").unwrap().as_str();
            let rhs = v.name("rhs").unwrap().as_str();

            Instruction { lhs, rhs }
        })
        .collect()
}
