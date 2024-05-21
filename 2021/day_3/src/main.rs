use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    let data: Vec<Vec<char>> = file_contents.lines().map(|v| v.chars().collect()).collect();

    part_one(&data);
    part_two(&data);
}

fn part_one(data: &[Vec<char>]) {
    let occurences = occurences_of('1', data);
    let gamma_rate = occurences.iter().enumerate().fold(0, |acc, (i, v)| {
        if *v > data.len() as u32 / 2 {
            return acc | 0b1 << (occurences.len() - 1 - i);
        }
        acc
    });
    let epsilon_rate = !gamma_rate & (2usize.pow(occurences.len() as u32 - 1) - 1);
    println!("Part One: {}", gamma_rate * epsilon_rate);
}

fn part_two(data: &[Vec<char>]) {
    let oxygen_rating = calc_rating(data, |v, mid| v >= mid);
    let c02_rating = calc_rating(data, |v, mid| v < mid);

    println!("Part Two: {}", oxygen_rating * c02_rating);
}

fn calc_rating(data: &[Vec<char>], f: fn(f32, f32) -> bool) -> isize {
    let mut nums = data.to_vec();
    for i in 0..nums[0].len() {
        let mut occurences_of_1 = 0.0;
        nums.iter()
            .filter(|&v| v[i] == '1')
            .for_each(|_| occurences_of_1 += 1.0);

        if f(occurences_of_1, nums.len() as f32 / 2.0) {
            nums.retain(|v| v[i] == '1');
        } else {
            nums.retain(|v| v[i] == '0');
        };

        if nums.len() <= 1 {
            break;
        }
    }

    isize::from_str_radix(&nums[0].iter().collect::<String>(), 2).unwrap()
}

fn occurences_of(target: char, nums: &[Vec<char>]) -> Vec<u32> {
    let mut occurences = vec![0; nums[0].len()];
    for idx in 0..nums[0].len() {
        nums.iter()
            .filter(|&v| v[idx] == target)
            .for_each(|_| occurences[idx] += 1)
    }
    occurences
}
