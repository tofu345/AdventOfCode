use regex::Regex;
use std::{collections::HashMap, fs};

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    // part_two(&file_contents);
}

#[derive(Debug, Clone)]
struct Node {
    val: String,
    left: String,
    right: String,
}

fn parse_input(file_contents: &str) -> (String, HashMap<String, Node>) {
    let mut nodes: HashMap<String, Node> = HashMap::new();
    let order = file_contents.lines().next().unwrap();

    let re = Regex::new(r"(?<v>\w{3}) = \((?<l>\w{3}), (?<r>\w{3})\)").unwrap();
    re.captures_iter(file_contents.strip_prefix(order).unwrap())
        .for_each(|caps| {
            let v = caps.name("v").unwrap().as_str();
            let l = caps.name("l").unwrap().as_str();
            let r = caps.name("r").unwrap().as_str();
            nodes.insert(
                v.to_owned(),
                Node {
                    val: v.to_owned(),
                    left: l.to_owned(),
                    right: r.to_owned(),
                },
            );
        });

    (order.to_owned(), nodes)
}

fn part_one(file_contents: &str) {
    let (order, nodes) = parse_input(file_contents);
    let mut curr = nodes.get("AAA").unwrap();
    let mut steps = 0;

    'outer: loop {
        for dir in order.chars() {
            if curr.val == "ZZZ" {
                break 'outer;
            }

            match dir {
                'L' => curr = nodes.get(&curr.left).unwrap(),
                'R' => curr = nodes.get(&curr.right).unwrap(),
                _ => unreachable!(),
            }
            steps += 1;
        }
    }

    println!("Part One: {}", steps);
}

fn part_two(file_contents: &str) {
    let (order, nodes) = parse_input(file_contents);
    let mut starting_nodes: Vec<Node> = nodes
        .values()
        .filter(|n| n.val.ends_with('A'))
        .cloned()
        .collect();
    let mut steps = 0;

    'outer: loop {
        for dir in order.chars() {
            if starting_nodes
                .iter()
                .filter(|n| n.val.ends_with('Z'))
                .count()
                == starting_nodes.len()
            {
                break 'outer;
            }

            for i in 0..starting_nodes.len() {
                // println!("{:#?}", starting_nodes);

                let node: &Node = starting_nodes.get(i).unwrap();

                starting_nodes[i] = match dir {
                    'L' => nodes.values().find(|n| n.val == node.left).unwrap().clone(),
                    'R' => nodes
                        .values()
                        .find(|n| n.val == node.right)
                        .unwrap()
                        .clone(),
                    _ => unreachable!(),
                }
            }

            steps += 1;
        }
    }

    println!("Part Two {:#?}", steps);
}
