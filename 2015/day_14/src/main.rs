use regex::Regex;
use std::{cmp::Ordering, fs};

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    let exp = r".*fly (?<fly_speed>.*) km/s.*for (?<fly_time>.*) seconds,.*for (?<rest_time>.*) seconds\.";
    let re = Regex::new(exp).unwrap();
    let reindeers: Vec<(i32, i32, i32)> = re
        .captures_iter(&file_contents)
        .map(|v| {
            let fly_speed: i32 = v.name("fly_speed").unwrap().as_str().parse().unwrap();
            let fly_time: i32 = v.name("fly_time").unwrap().as_str().parse().unwrap();
            let rest_time: i32 = v.name("rest_time").unwrap().as_str().parse().unwrap();
            (fly_speed, fly_time, rest_time)
        })
        .collect();

    part_one(&reindeers);
    part_two(&reindeers);
}

fn part_one(reindeers: &Vec<(i32, i32, i32)>) {
    let duration = 2503;
    let mut largest_distance = 0;
    for &(fly_speed, fly_time, rest_time) in reindeers {
        let mut distance = 0;
        let mut rest_time_left = 0;
        let mut fly_time_left = fly_time;
        for _ in 0..duration {
            if rest_time_left > 0 {
                rest_time_left -= 1;
                continue;
            }

            distance += fly_speed;
            fly_time_left -= 1;
            if fly_time_left == 0 {
                rest_time_left = rest_time;
                fly_time_left = fly_time;
            }
        }

        largest_distance = std::cmp::max(distance, largest_distance);
    }

    println!("Part One: {}", largest_distance);
}

#[derive(Debug)]
struct Reindeer {
    distance: i32,
    fly_speed: i32,
    fly_time: i32,
    rest_time: i32,
    fly_time_left: i32,
    rest_time_left: i32,
    points: i32,
}

impl Reindeer {
    fn new(fly_speed: i32, fly_time: i32, rest_time: i32) -> Self {
        Reindeer {
            distance: 0,
            fly_speed,
            fly_time,
            rest_time,
            fly_time_left: fly_time,
            rest_time_left: 0,
            points: 0,
        }
    }
}

// Dont fix it if it aint broke.
// My advice to you, my future self.
// For context, I feel like this code is bloated but its 00:24 rn so I could not care less
fn part_two(reindeers: &[(i32, i32, i32)]) {
    let duration = 2503;
    let mut reindeers: Vec<Reindeer> = reindeers
        .iter()
        .map(|&(fly_speed, fly_time, rest_time)| Reindeer::new(fly_speed, fly_time, rest_time))
        .collect();

    for _ in 0..duration {
        for deer in reindeers.iter_mut() {
            if deer.rest_time_left > 0 {
                deer.rest_time_left -= 1;
                continue;
            }

            deer.distance += deer.fly_speed;
            deer.fly_time_left -= 1;
            if deer.fly_time_left == 0 {
                deer.rest_time_left = deer.rest_time;
                deer.fly_time_left = deer.fly_time;
            }
        }

        let mut deers_in_lead: Vec<&mut Reindeer> = Vec::new();
        for deer in reindeers.iter_mut() {
            let lead_distance = deers_in_lead.first().map(|v| v.distance).unwrap_or(0);
            match deer.distance.cmp(&lead_distance) {
                Ordering::Equal => deers_in_lead.push(deer),
                Ordering::Greater => deers_in_lead = vec![deer],
                Ordering::Less => (),
            }
        }

        deers_in_lead.into_iter().for_each(|v| v.points += 1);
    }

    let mut winner = &reindeers[0];
    for deer in reindeers.iter() {
        if deer.points > winner.points {
            winner = &deer;
        }
    }
    println!("Part Two: {}", winner.points);
}
