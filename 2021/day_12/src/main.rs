use std::{collections::HashMap, fs, io};

fn main() -> io::Result<()> {
    let file_contents = fs::read_to_string("test.txt")?;

    let data = parse(&file_contents);
    part_one(&data);
    // part_two(&file_contents);
    Ok(())
}

fn parse(file_contents: &str) -> Caves {
    let mut data: Caves = HashMap::new();
    for line in file_contents.lines() {
        let splits: Vec<&str> = line.split('-').collect();
        if splits[1] == "start" || splits[0] == "end" {
            data.entry(splits[1])
                .and_modify(|v| v.push(splits[0]))
                .or_insert(vec![splits[0]]);
        } else {
            data.entry(splits[0])
                .and_modify(|v| v.push(splits[1]))
                .or_insert(vec![splits[1]]);
        }
    }
    println!("{data:#?}");
    data
}

type Caves<'a> = HashMap<&'a str, Vec<&'a str>>;

fn part_one(caves: &Caves) {
    fn recurse<'a>(
        caves: &'a Caves,
        curr: &'a str,
        mut curr_path: Vec<&'a str>,
        paths: &mut Vec<Vec<&'a str>>,
    ) {
        if curr == "end" {
            curr_path.push(curr);
            paths.push(curr_path);
            return;
        };

        if ('a'..='z').any(|v| v == curr.chars().next().unwrap()) && curr_path.contains(&curr) {
            return;
        }
        // println!("{curr_path:?} {paths:?}");

        let Some(connections) = caves.get(curr) else {
            return;
        };
        let reverse_connections: Vec<&str> = caves
            .iter()
            .filter(|(_, v)| v.contains(&curr))
            .map(|(&k, _)| k)
            .filter(|&v| v != "end")
            .collect();

        for c in connections {
            let mut next_path = curr_path.clone();
            next_path.push(curr);
            recurse(caves, c, next_path, paths);
        }
        for c in reverse_connections {
            let mut next_path = curr_path.clone();
            next_path.push(curr);
            recurse(caves, c, next_path, paths);
        }
    }

    let mut paths = vec![];
    recurse(caves, "start", vec![], &mut paths);
    println!("{:#?}", paths.len());
}

fn part_two(file_contents: &str) {
    todo!()
}
