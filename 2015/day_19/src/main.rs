use std::collections::{HashMap, HashSet};
use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("test.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    // part_two(&file_contents);
}

fn part_one(file_contents: &str) {
    let lines: Vec<_> = file_contents.lines().collect();
    let len = lines.len();
    let mut distinct_molecules: HashSet<String> = HashSet::new();
    let molecule = lines[len - 1];
    for &line in lines.iter().take(len - 2) {
        let splits: Vec<_> = line.split(" => ").collect();
        let occurences: Vec<_> = molecule.match_indices(splits[0]).map(|(i, _)| i).collect();
        println!("{:?}", occurences);
        for i in occurences {
            distinct_molecules.insert(replace_substr(molecule, splits[1], i, splits[0].len()));
        }
    }

    println!("{:?}", distinct_molecules);
    println!("Part One: {}", distinct_molecules.len());
}

type Reps<'a> = HashMap<&'a str, &'a str>;

fn part_two(file_contents: &str) {
    let target = file_contents.lines().last().unwrap();
    let data: Vec<_> = file_contents
        .lines()
        .take(file_contents.lines().count() - 2)
        .map(|v| {
            let splits: Vec<&str> = v.split(" => ").collect();
            (splits[1], splits[0])
        })
        .collect();
    let electrons: Reps = HashMap::from_iter(data.iter().filter(|(_, v)| v.contains('e')).copied());
    let reps: Reps = HashMap::from_iter(data.into_iter().filter(|(_, v)| !v.contains('e')));

    let mut queue: Vec<Node> = vec![Node::new(target.to_owned(), &reps)];
    let mut min_steps = None;
    // for _ in 0..3 {
    while !queue.is_empty() {
        let len = queue.len();
        let curr = &queue[len - 1];
        println!("{curr:?}");

        if electrons.get(&*queue[len - 1].data).is_some() {
            min_steps = Some(std::cmp::min(min_steps.unwrap_or(len), len));
            println!(
                "Reached target {} {} {:?}",
                queue[len - 1].data,
                queue.len(),
                queue.iter().map(|v| &v.data).collect::<Vec<&String>>()
            );
            queue.pop();
            continue;
        }

        if curr.idx >= curr.matches.len() {
            queue.pop();
            continue;
        }

        let curr_match = &curr.matches[curr.idx];
        let curr_reps = reps.get(&*curr_match.1).unwrap();
        let next = replace_substr(&curr.data, curr_reps, curr_match.0, curr_match.1.len());
        queue[len - 1].idx += 1;
        queue.push(Node::new(next, &reps));
    }

    println!("Part Two: {:?}", min_steps);
}

fn replace_substr(data: &str, substr: &str, idx: usize, len: usize) -> String {
    let mut out = String::with_capacity(data.len());
    data.chars().take(idx).for_each(|ch| out.push(ch));
    out.push_str(substr);
    data.chars().skip(idx + len).for_each(|ch| out.push(ch));
    out
}

#[derive(Debug)]
struct Node {
    data: String,
    matches: Vec<(usize, String)>,
    idx: usize,
}

impl Node {
    fn new(val: String, reps: &Reps) -> Self {
        let mut possible_reps = Vec::new();
        for &k in reps.keys() {
            let matches: Vec<(usize, String)> = val
                .match_indices(k)
                .map(|(i, v)| (i, v.to_owned()))
                .collect();
            // println!("{k} {v} {matches:?}");
            if !matches.is_empty() {
                possible_reps.extend(matches);
            }
        }
        // println!("{possible_reps:?} {val}");
        Node {
            data: val,
            matches: possible_reps,
            idx: 0,
        }
    }
}
