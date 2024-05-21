use std::{cmp, collections::HashMap, fs};

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    part_two(&file_contents);
}

#[derive(Debug)]
struct Game {
    id: u32,
    draws: Vec<HashMap<String, u32>>,
}

fn parse_games(file_contents: &str) -> Vec<Game> {
    let mut games: Vec<Game> = Vec::new();

    for line in file_contents.lines() {
        let splits: Vec<&str> = line.split(": ").collect();
        let id: u32 = splits[0].split(" ").last().unwrap().parse().unwrap();

        let mut draws: Vec<HashMap<String, u32>> = Vec::new();
        let data: Vec<&str> = splits[1].split("; ").collect();
        for draw in data {
            let mut map: HashMap<String, u32> = HashMap::new();
            for cube_draw in draw.split(", ") {
                let s: Vec<&str> = cube_draw.split(" ").collect();
                let num_cubes: u32 = s[0].parse().unwrap();

                map.entry(s[1].to_owned())
                    .and_modify(|e| *e += num_cubes)
                    .or_insert(num_cubes);
            }

            draws.push(map)
        }

        games.push(Game { id, draws });
    }

    games
}

fn part_one(file_contents: &str) {
    let games = parse_games(file_contents);
    let (max_red, max_green, max_blue) = (12u32, 13u32, 14u32);
    let mut possible_games: u32 = 0;

    'outer: for game in games {
        for draw in game.draws.iter() {
            if *draw.get("red").unwrap_or(&0u32) > max_red
                || *draw.get("green").unwrap_or(&0u32) > max_green
                || *draw.get("blue").unwrap_or(&0u32) > max_blue
            {
                continue 'outer;
            }
        }

        possible_games += game.id;
    }

    println!("Part One: {possible_games}");
}

fn part_two(file_contents: &str) {
    let games = parse_games(file_contents);
    let mut power: u32 = 0;

    for game in games {
        let mut max_red = 0;
        let mut max_green = 0;
        let mut max_blue = 0;

        for draw in game.draws.iter() {
            draw.get("red").map(|&v| max_red = cmp::max(v, max_red));
            draw.get("green")
                .map(|&v| max_green = cmp::max(v, max_green));
            draw.get("blue").map(|&v| max_blue = cmp::max(v, max_blue));
        }

        power += max_red * max_green * max_blue;
    }

    println!("Part Two: {power}");
}
