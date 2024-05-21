use std::{collections::VecDeque, fmt::Display, fs, time::SystemTime, usize};

const DAYS: u64 = 80;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    println!("Be prepared to wait 1h30mins");
    part_one(&file_contents);

    let curr_time = SystemTime::now();
    part_two(&file_contents, 256);
    println!("Congrats you only had to wait {:?}", curr_time.elapsed().unwrap());
}

fn part_one(file_contents: &str) {
    let mut fishes: Vec<i32> = file_contents
        .trim()
        .split(',')
        .map(|v| v.parse().unwrap())
        .collect();
    for _ in 0..DAYS {
        fishes.iter_mut().for_each(|v| *v -= 1);
        let idx_new: Vec<usize> = fishes
            .iter()
            .enumerate()
            .filter(|(_, &v)| v < 0)
            .map(|(i, _)| i)
            .collect();

        idx_new.into_iter().for_each(|i| {
            fishes[i] = 6;
            fishes.push(8);
        });
    }

    println!("Part One: {}", fishes.len());
}

fn part_two(file_contents: &str, days: u64) {
    let mut fishes: VecDeque<Fish> = file_contents
        .trim()
        .split(',')
        .map(|v| Fish {
            day: v.parse().unwrap(),
            is_new: false,
        })
        .collect();
    let mut num_fishes = fishes.len();

    // good luck understanding this
    while !fishes.is_empty() {
        let curr = fishes.pop_back().unwrap();
        for v in 0.. {
            let mut day = 1 + curr.day + (7 * v);
            if curr.is_new {
                if v == 0 {
                    continue;
                }
                day += 1;
            }

            if day > days {
                break;
            }

            num_fishes += 1;
            fishes.push_back(Fish { day, is_new: true });
        }
    }

    println!("Part Two: {}", num_fishes);
}

#[derive(Debug)]
struct Fish {
    day: u64,
    is_new: bool,
}

impl Display for Fish {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:({})", if self.is_new { "N" } else { "-" }, self.day,)
    }
}

#[allow(unused)]
fn print_fishes(fishes: &VecDeque<Fish>) {
    print!("[");
    for (i, fish) in fishes.iter().enumerate() {
        print!("{fish}");
        if i != fishes.len() - 1 {
            print!(", ");
        }
    }
    println!("]");
}
