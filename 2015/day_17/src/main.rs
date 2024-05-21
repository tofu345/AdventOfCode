use std::fs;

// https://youtu.be/-L5j1ZZTlVQ?si=42WaKsxs2NSmBKIK
// didn't know what to do so used that vid as a guide

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    let data: Vec<i32> = file_contents
        .lines()
        .map(|v| v.parse::<i32>().unwrap())
        .collect();

    part_one(&data);
    part_two(&data);
}

fn part_one(data: &[i32]) {
    println!(
        "Part One: {}",
        combinations(data)
            .iter()
            .filter(|v| sum_eggnog(v) == 150)
            .count()
    );
}

fn part_two(data: &[i32]) {
    let combos: Vec<usize> = combinations(data)
        .iter()
        .filter(|v| sum_eggnog(v) == 150)
        .map(|v| v.len())
        .collect();
    let min = *combos.iter().min().unwrap();
    println!("Part Two: {}", combos.iter().filter(|&&v| v == min).count());
}

fn sum_eggnog(idx: &[i32]) -> i32 {
    idx.iter().fold(0, |acc, v| acc + *v)
}

fn combinations(vec: &[i32]) -> Vec<Vec<i32>> {
    let mut combos: Vec<Vec<i32>> = Vec::new();
    for idx in 1..=vec.len() {
        let mut combo = Vec::with_capacity(idx);
        for i in 0..idx {
            combo.push(i);
        }

        combos.push(combo.iter().map(|i| vec[*i]).collect());

        loop {
            let mut done = true;
            'inc: for inc_index in (0..idx).rev() {
                combo[inc_index] += 1;
                if combo[inc_index] == vec.len() {
                    continue;
                }

                for next in inc_index + 1..idx {
                    combo[next] = combo[next - 1] + 1;
                    if combo[next] == vec.len() {
                        continue 'inc;
                    }
                }
                done = false;
                break;
            }

            if !done {
                combos.push(combo.iter().map(|i| vec[*i]).collect());
            } else {
                break;
            }
        }
    }

    combos
}
